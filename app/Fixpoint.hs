{-# LANGUAGE TemplateHaskell #-}

module Fixpoint where

import Control.Lens hiding (para)
import Data.Map.Merge.Lazy (mergeA, traverseMissing, zipWithAMatched)
import Data.Map.Monoidal (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Egraph (Ex1 (..))
import Lude

class (Monoid (Delta d)) => CSL d where
  type Delta d
  cslBottom :: d
  cslIsBottom :: d -> Bool
  cslIsBottom d = cslDeltaIsZero (Proxy @d) . fst $ cslMerge d cslBottom

  -- Note: the second argument is just a commutative semilattice operation, but
  -- the delta is asymmetric: it expected that we have some monoidal action
  -- apply such that apply (fst (cslMerge a b)) a = b
  cslMerge :: d -> d -> (Delta d, d)

  -- Proxy is needed because of annoyances in type inference
  cslDeltaIsZero :: Proxy d -> Delta d -> Bool

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

unionAllEqrel :: (Ord v) => [v] -> Eqrel v -> Eqrel v
unionAllEqrel [] er = er
unionAllEqrel (i : is) er = foldr (unionEqrel' i) er is

instance (Ord v) => Semigroup (Eqrel v) where
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

makeLenses ''AnnEqrel

instance (Ord v, CSL a) => CSL (AnnEqrel v a) where
  type Delta (AnnEqrel v a) = (Delta (Eqrel v), MonoidalMap v (Delta a))
  cslBottom = AnnEqrelC cslBottom cslBottom
  cslIsBottom (AnnEqrelC er an) = cslIsBottom er && cslIsBottom an
  cslMerge e1 e2 =
    let (changedE, newE) = cslMerge (e1 ^. eqrel) (e2 ^. eqrel)
        (changedA, newA) = cslMerge (e1 ^. annotations) (e2 ^. annotations)
     in usingState (AnnEqrelC newE newA) do
          s <- for changedE \(a, c) -> do
            oldA <- annotations . at a <<.= Nothing
            d <- annotations . at c . anon cslBottom cslIsBottom %%= cslMerge (fromMaybe cslBottom oldA)
            pure (c, d)
          pure (changedE, changedA <> fromList s)
  cslDeltaIsZero _ (d, m) = cslDeltaIsZero (Proxy @(Eqrel v)) d && cslDeltaIsZero (Proxy @(Map v a)) m

-- nondeterministic rewrite system, interpretation over eqrels
rwSystem :: (Functor f, Ord (f Int)) => Map (f Int) [Int] -> Eqrel Int -> Eqrel Int
rwSystem m e =
  foldr unionAllEqrel e $
    getMonoidalMap $
      ifoldMap
        (\fi is -> [(fmap (findEqrel e) fi, is)])
        m

-- merge over empty, f(empty), ... f^i(empty), etc
cslFixpoint :: forall a. (CSL a) => (a -> a) -> a
cslFixpoint f = go cslBottom
  where
    -- make sure to put the arguments the right way! cslMerge (f a) a, not vice versa
    go a = let (d, a') = cslMerge (f a) a in if cslDeltaIsZero (Proxy @a) d then a' else go a'

-- magic! ...ish
congruenceClosure :: (Functor f, Ord (f Int)) => Map (f Int) [Int] -> Eqrel Int
congruenceClosure m = cslFixpoint (rwSystem m)

example4 :: Map (Ex1 Int) [Int]
example4 =
  fromList
    [ (H 1, [2, 3]),
      (F 1 2, [4]),
      (F 1 3, [5]),
      (G 4, [6]),
      (G 5, [5])
    ]