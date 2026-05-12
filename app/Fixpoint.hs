{-# LANGUAGE TemplateHaskell #-}

module Fixpoint where

import Control.Lens hiding (para)
import Control.Monad.Writer (MonadWriter (..))
import Data.Foldable (foldrM)
import Data.Map.Merge.Lazy (mergeA, traverseMissing, zipWithAMatched)
import Data.Map.Monoidal (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Egraph (EId, Ex1 (..))
import Lude

-- 'Computational semilattice'
-- Probably could do with a better name
class (Monoid (Delta d)) => CSL d where
  type Delta d
  cslBottom :: d
  cslIsBottom :: d -> Bool
  cslIsBottom d = cslDeltaIsZero (Proxy @d) . fst $ cslMerge d cslBottom

  -- Note: the second part of the result tuple is just from a commutative
  -- semilattice operation, but
  -- the delta is asymmetric: it expected that we have some monoidal action
  -- apply such that apply (fst (cslMerge a b)) a = b
  cslMerge :: d -> d -> (Delta d, d)

  -- Proxy is needed because of annoyances in type inference
  cslDeltaIsZero :: Proxy d -> Delta d -> Bool

type DeltaF d = d -> (Delta d, d)

-- Pointwise CSL
instance (Ord b, CSL d) => CSL (Map b d) where
  -- The right monoid instance comes from MonoidalMap
  type Delta (Map b d) = MonoidalMap b (Delta d)
  cslBottom = Map.empty
  cslIsBottom = all cslIsBottom
  cslMerge =
    mergeA
      (traverseMissing (const (mempty,)))
      (traverseMissing (const (mempty,)))
      ( zipWithAMatched
          ( \b x y ->
              let (d, o) = cslMerge x y in ([(b, d)], o)
          )
      )
  cslDeltaIsZero _ = all (cslDeltaIsZero (Proxy @d))

data Eqrel v
  = EqrelC
  { _erRoot :: Map v v,
    _erBack :: Map v (Set v)
  }
  deriving (Show)

makeLenses ''Eqrel

findEqrel :: (Ord v) => Eqrel v -> v -> v
findEqrel e v = e ^. erRoot . at v . non v

unionEqrel :: (Ord v) => v -> v -> Eqrel v -> (Bool, Eqrel v)
unionEqrel a b = runState do
  rb <- use (erRoot . at b . non b)
  ra <- erRoot . at a . non a <<.= rb
  if ra /= rb
    then do
      sa <- erBack . at ra . anon Set.empty Set.null <<.= Set.empty
      for_ sa (\y -> erRoot . at y ?= rb)
      erBack . at ra .= Nothing
      erBack . at rb . anon Set.empty Set.null <>= one ra <> sa
      pure True
    else pure False

unionEqrel' :: (Ord v) => v -> v -> Eqrel v -> Eqrel v
unionEqrel' a b e = snd (unionEqrel a b e)

unionEqrelD :: (Ord v) => v -> v -> DeltaF (Eqrel v)
unionEqrelD a b e = let (d, e') = unionEqrel a b e in ([(a, b) | d], e')

unionAllEqrelD :: (Ord v) => [v] -> DeltaF (Eqrel v)
unionAllEqrelD [] er = pure er
unionAllEqrelD (i : is) er = foldrM (unionEqrelD i) er is

instance (Ord v) => Semigroup (Eqrel v) where
  (<>) :: (Ord v) => Eqrel v -> Eqrel v -> Eqrel v
  a <> b = snd (cslMerge a b)

instance (Ord v) => Monoid (Eqrel v) where
  mempty = cslBottom

-- Equivalence relations are CSLs
instance (Ord v) => CSL (Eqrel v) where
  -- question: should this be [(v, v)]? Is the resulting Delta 'nice' enough
  -- as a normal Eqrel?
  type Delta (Eqrel v) = [(v, v)]
  cslBottom = EqrelC Map.empty Map.empty
  cslIsBottom (EqrelC m _) = Map.null m
  cslMerge m1 m2 = executingState ([], m2) do
    ifor (m1 ^. erRoot) \c r -> do
      -- did we have to update m2? If so, it is an equivalence in m1 that is not
      -- in m2
      b <- _2 %%= unionEqrel c r
      when b (_1 <|= (c, r))
  cslDeltaIsZero _ = null

-- A kind of dependent product or semidirect product of Eqrel v and Map v a
data AnnEqrel v a
  = AnnEqrelC
  { _eqrel :: Eqrel v,
    _annotations :: Map v a
  }
  deriving (Show)

makeLenses ''AnnEqrel

annotationOf :: (CSL a, Ord v) => v -> Lens' (AnnEqrel v a) a
annotationOf c = annotations . at c . anon cslBottom cslIsBottom

updateAnnotation :: (CSL a, Ord v) => v -> a -> DeltaF (AnnEqrel v a)
updateAnnotation v a ae = let (d, ae') = ae & annotationOf v %%~ cslMerge a in ((mempty, [(v, d)]), ae')

unionAllAnnEqrelD :: (CSL a, Ord v) => ([v], a) -> DeltaF (AnnEqrel v a)
unionAllAnnEqrelD ([], _) ae = pure ae
unionAllAnnEqrelD (i : is, a) ae = do
  ae' <- first (,mempty) $ ae & eqrel %%~ unionAllEqrelD (i : is)
  updateAnnotation i a ae'

instance (Ord v, CSL a) => CSL (AnnEqrel v a) where
  type Delta (AnnEqrel v a) = (Delta (Eqrel v), MonoidalMap v (Delta a))
  cslBottom = AnnEqrelC cslBottom cslBottom
  cslIsBottom (AnnEqrelC er an) = cslIsBottom er && cslIsBottom an
  cslMerge e1 e2 =
    let (changedE, newE) = cslMerge (e1 ^. eqrel) (e2 ^. eqrel)
        (changedA, newA) = cslMerge (e1 ^. annotations) (e2 ^. annotations)
     in usingState (AnnEqrelC newE newA) do
          s <- for changedE \(a, c) -> do
            oldA <- annotationOf a <<.= cslBottom
            d <- annotationOf c %%= cslMerge oldA
            pure (c, d)
          pure (changedE, changedA <> fromList s)
  cslDeltaIsZero _ (d, m) = cslDeltaIsZero (Proxy @(Eqrel v)) d && cslDeltaIsZero (Proxy @(Map v a)) m

congruentMerge :: (Functor f, Ord v, Ord (f v), Monoid a) => Map (f v) a -> Eqrel v -> Map (f v) a
congruentMerge m e = getMonoidalMap $ ifoldMap (\fi a -> [(fmap (findEqrel e) fi, a)]) m

-- nondeterministic rewrite system, interpretation over eqrels
rwSystem :: (Functor f, Ord v, Ord (f v)) => Map (f v) [v] -> DeltaF (Eqrel v)
rwSystem m e = foldrM unionAllEqrelD e $ congruentMerge m e

detrwSystem :: (Functor f, Ord v, Ord (f v)) => Map (f v) v -> DeltaF (Eqrel v)
detrwSystem m e = foldrM unionAllEqrelD e $ congruentMerge (fmap one m) e

-- merge over empty, f(empty), ... f^i(empty), etc
cslFixpoint :: forall a. (CSL a) => DeltaF a -> a
cslFixpoint f = go cslBottom
  where
    go a = let (d, a') = f a in if cslDeltaIsZero (Proxy @a) d then a' else go a'

-- magic! ...ish
congruenceClosure :: (Functor f, Ord (f Int)) => Map (f Int) [Int] -> Eqrel Int
congruenceClosure m = cslFixpoint (rwSystem m)

-- CSL cslMerge is a bit awkward to compose, here's a helper
data DeltaM a = DeltaM (Delta a) a

instance (CSL a) => Semigroup (DeltaM a) where
  DeltaM d1 a1 <> DeltaM d2 a2 = let (d, a) = cslMerge a1 a2 in DeltaM (d1 <> d2 <> d) a

instance (CSL a) => Monoid (DeltaM a) where
  mempty = DeltaM mempty cslBottom

-- the first two arguments are morally f (Int, a) -> (Int, a)
-- but this is only finitely presentable via a map on the Int part
annRwSystem :: (CSL a, Functor f, Ord v, Ord (f v)) => Map (f v) v -> (f a -> a) -> DeltaF (AnnEqrel v a)
annRwSystem m f ae =
  foldrM
    -- is throwing the delta away here ok?
    (\(is, DeltaM _ a) -> unionAllAnnEqrelD (is, a))
    ae
    $ congruentMerge
      (imap (\fi i -> ([i], DeltaM mempty (upd fi) <> DeltaM mempty (ae ^. annotationOf i))) m)
      (ae ^. eqrel)
  where
    upd = f . fmap (\i -> ae ^. annotationOf i)

annotatedCongruenceClosure :: (CSL a, Functor f, Ord v, Ord (f v)) => Map (f v) v -> (f a -> a) -> AnnEqrel v a
annotatedCongruenceClosure m f = cslFixpoint (annRwSystem m f)

example3 :: DeltaF (Eqrel Int)
example3 e =
  -- we can never generate a nontrivial congruence relation from just a deterministic rewrite system!
  -- H 1 -> 2 will be given priority
  detrwSystem
    ( fromList
        [ (H 1, 2),
          (H 1, 3),
          (F 1 2, 4),
          (F 1 3, 5),
          (G 4, 6),
          (G 5, 5)
        ]
    )
    -- ...so we fix it here
    (unionEqrel' 2 3 e)

example4 :: Map (Ex1 Int) [Int]
example4 =
  fromList
    [ (H 1, [2, 3]),
      (F 1 2, [4]),
      (F 1 3, [5]),
      (G 4, [6]),
      (G 5, [5])
    ]

example5 :: Ex1 Text -> Text
example5 (F a b) = "(F" <> a <> " " <> b <> ")"
example5 (G a) = "(G" <> a <> ")"
example5 (H i) = "(H" <> show i <> ")"

instance CSL Text where
  type Delta Text = Any
  cslBottom = ""
  cslDeltaIsZero _ = getAny
  cslMerge a b = if a == "" then (mempty, b) else (Any True, a <> b)

-- requirements to make a more efficient version
-- 1. Eqrel should be specialized to EqrelInt with IntMaps.
-- 2. The same with the other uses for Map/MonoidalMap
-- 3. We need a 'uses' map, which is a kind of E-analysis in this hopelessly
--   general framework

data CCFast f
  = CCFastC {_ccSlow :: AnnEqrel EId (Set (f EId))}

makeLenses ''CCFast