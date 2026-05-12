{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use lambda-case" -}

module Egraph
  ( EId (..),
    unId,
    Signature (..),
    Egraph,
    efind,
    eunion,
    Ex1(..),
  )
where

import Control.Lens hiding (para)
import Control.Monad.Free (Free (..), iter)
import Data.Fix (Fix (Fix))
import Data.Functor.Foldable (cata)
import Data.IntMap qualified as IntMap
import Data.Map.Merge.Lazy (mergeA, traverseMissing, zipWithAMatched)
import Data.Map.Monoidal (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lude
import Prettyprinter (Doc, indent, line, viaShow, vsep, (<+>))

newtype EId = Id {_unId :: Int}
  deriving (Eq, Ord, Hashable, Show, Read, Generic)

makeWrapped ''EId

instance Bounded EId where
  minBound = Id 0
  maxBound = Id maxBound

makeLenses ''EId

class (Ord (Symbol enode), Ord (enode EId), Traversable enode) => Signature enode where
  type Symbol enode

  symbolOf :: enode a -> Symbol enode
  default symbolOf :: (Symbol enode ~ enode ()) => enode a -> Symbol enode
  symbolOf = void

-- reconstruct :: Symbol enode -> [a] -> Maybe (enode a)
-- default reconstruct :: (Symbol enode ~ enode ()) => Symbol enode -> [a] -> Maybe (enode a)
-- reconstruct s as = sequence . (partsOf traverse .~ fmap Just as) . (Nothing <$) $ s

data Use c
  = LeftUse c
  | RightUse c
  deriving (Eq, Ord)

data Egraph enode ann
  = EgraphC
  { _ufRoot :: IntMap EId,
    _ufNext :: EId,
    _egForward :: Map (enode EId) EId,
    _egAnn :: IntMap (ann, Set (Use (enode EId)))
  }

makeLenses ''Egraph

efind :: EId -> Lens' (Egraph enode ann) EId
efind e = ufRoot . at (e ^. unId) . anon e (== e)

class (Signature (EnodeOf ann)) => Annotation ann where
  type EnodeOf ann :: Type -> Type
  annEmpty :: ann
  annIsEmpty :: ann -> Bool
  annMerge :: ann -> ann -> (ann, Bool)
  annF :: EnodeOf ann ann -> (ann, Bool)

instance (Annotation a, Annotation b, EnodeOf a ~ EnodeOf b) => Annotation (a, b) where
  type EnodeOf (a, b) = EnodeOf a
  annEmpty = (annEmpty, annEmpty)
  annIsEmpty (a, b) = annIsEmpty a && annIsEmpty b
  annMerge (a1, b1) (a2, b2) | (a, da) <- annMerge a1 a2, (b, db) <- annMerge b1 b2 = ((a, b), da || db)
  annF en
    | (a, da) <- annF (fst <$> en),
      (b, db) <- annF (snd <$> en) =
        ((a, b), da || db)

eannotation :: (Annotation ann) => EId -> Lens' (Egraph enode ann) (ann, Set (Use (enode EId)))
eannotation e = egAnn . at (e ^. unId) . anon (annEmpty, Set.empty) (\(a, s) -> annIsEmpty a && Set.null s)

eunion :: (Signature (EnodeOf ann), Annotation ann) => EId -> EId -> State (Egraph (EnodeOf ann) ann) EId
eunion a b = do
  a' <- use (efind a)
  b' <- use (efind b)
  if a' == b'
    then pure a'
    else do
      (annA, usesA) <- use (eannotation a')
      (annB, usesB) <- use (eannotation b')
      let (newAnn, needsChange) = annMerge annA annB
      let (newRoot, newChild) =
            if length usesA < length usesB
              then (b', a')
              else (a', b')
      ufRoot . at (newChild ^. unId) ?= newRoot
      egAnn . at (newChild ^. unId) .= Nothing
      eannotation newRoot .= (newAnn, usesA <> usesB)
      -- At this point, newChild is a noncanonical id pointing to newRoot,
      -- and the annotations for newRoot have been updated. However, we need to find all
      -- uses of newChild and change them to newRoot, which might recursively cause more merges.
      -- Also, we need to find uses of newRoot to note that the annotation has been updated
      -- (if needsChange is true)
      for_ usesA \case
        LeftUse en -> do
          let en' = fmap (\e -> if e == newChild then newRoot else e) en

          rA <- use (egForward . at en)
          rB <- use (egForward . at en')
          whenJust rA \s -> do
            egForward . at en .= Nothing
            eannotation s . _2 . contains (RightUse en) .= False
            egForward . at en' ?= s
            eannotation s . _2 . contains (RightUse en') .= True
            whenJust rB $ void . eunion s
        RightUse en -> egForward . at en ?= newRoot

      when needsChange $ ereannotate newRoot
      pure newRoot

ereannotate :: (Signature (EnodeOf ann), Annotation ann) => EId -> State (Egraph (EnodeOf ann) ann) ()
ereannotate e =
  use (eannotation e . _2) >>= traverse_ \case
    LeftUse en -> do
      (newAnn, needsChange) <- annF <$> traverse (\e' -> use (eannotation e' . _1)) en
      when needsChange $ do
        eannotation e . _1 .= newAnn
        whenJustM (use (egForward . at en)) ereannotate
    RightUse _ -> pass

eupdate :: (Signature (EnodeOf ann), Annotation ann) => EId -> State ann b -> State (Egraph (EnodeOf ann) ann) b
eupdate e upd = (eannotation e . _1 %%= runState upd) <* ereannotate e

-- Pair stuff
-- The idea is that Fix (Currify t) is roughly ~ Fix t
data Currify t a
  = CyPair a a
  | CyHead (Symbol t)
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq a, Signature t) => Eq (Currify t a)

deriving instance (Ord a, Signature t) => Ord (Currify t a)

instance (Signature t) => Signature (Currify t) where
  type Symbol (Currify t) = Maybe (Symbol t)
  symbolOf :: (Signature t) => Currify t a -> Symbol (Currify t)
  symbolOf (CyPair _ _) = Nothing
  symbolOf (CyHead s) = Just s

reconstruct :: forall t a. (Symbol t ~ t (), Traversable t) => Free (Currify t) a -> Maybe (Free t a)
reconstruct (Pure s) = Just (Pure s)
reconstruct (Free cs) = go cs >>= (\(s, as) -> Free <$> traverse reconstruct (remake s as))
  where
    go (CyPair a (Free b)) = second (a :) <$> go b
    go (CyPair _ (Pure _)) = Nothing
    go (CyHead s) = Just (s, [])

    remake s as = s & unsafePartsOf traverse .~ as

edebug :: (a -> Doc ann) -> (e EId -> Doc ann) -> Egraph e a -> Doc ann
edebug showAnn showNode eg =
  "Egraph with"
    <+> showId (eg ^. ufNext)
    <+> "ids"
      <> line
      <> indent
        2
        ( vsep
            [ "ufRoot"
                <+> indent
                  2
                  ( vsep
                      $ fmap (\(i, Id j) -> viaShow i <+> "→" <+> viaShow j)
                      $ itoList (eg ^. ufRoot)
                  ),
              "egForward"
                <+> indent
                  2
                  ( vsep
                      $ fmap (\(lhs, rhs) -> showNode lhs <+> "→" <+> showId rhs)
                      $ itoList (eg ^. egForward)
                  ),
              "egAnn"
                <+> indent
                  2
                  ( vsep
                      $ fmap
                        ( \(i, (a, u)) ->
                            viaShow i
                              <+> "has annotation"
                              <+> showAnn a
                              <+> "and is used in"
                                <> line
                                <> indent
                                  2
                                  ( vsep
                                      ( fmap
                                          ( \case
                                              LeftUse en -> "a rule" <+> showNode en <+> "→ ?"
                                              RightUse en -> "the rule" <+> showNode en <+> "→" <+> viaShow i
                                          )
                                          $ toList u
                                      )
                                  )
                        )
                      $ itoList
                        (eg ^. egAnn)
                  )
            ]
        )
  where
    showId i = viaShow (i ^. unId)

einsert :: (Signature (EnodeOf ann), Annotation ann) => EnodeOf ann EId -> State (Egraph (EnodeOf ann) ann) EId
einsert en = do
  en' <- traverse (use . efind) en
  use (egForward . at en') >>= \case
    Nothing -> do
      n <- ufNext <<%= (unId +~ 1)
      egForward . at en' ?= n
      e' <- traverse (\e -> use (eannotation e . _1)) en'
      eannotation n .= (fst (annF e'), one $ RightUse en')
      for_ en' (\e'' -> eannotation e'' . _2 . contains (LeftUse en') .= True)
      pure n
    Just e -> pure e

--

einsertFix :: (Signature (EnodeOf ann), Annotation ann) => Fix (EnodeOf ann) -> State (Egraph (EnodeOf ann) ann) EId
einsertFix = cata (sequence >=> einsert)

einsertFree :: (Signature (EnodeOf ann), Annotation ann) => Free (EnodeOf ann) EId -> State (Egraph (EnodeOf ann) ann) EId
einsertFree = iter (sequence >=> einsert) . fmap pure

eempty :: Egraph enode ann
eempty = EgraphC IntMap.empty (Id 0) Map.empty IntMap.empty

data Ex1 a
  = F a a
  | G a
  | H Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Signature Ex1 where
  type Symbol Ex1 = Ex1 ()

-- a use for higher-kinded Proxy!
instance (Signature enode) => Annotation (Proxy (enode :: Type -> Type)) where
  type EnodeOf (Proxy enode) = enode
  annEmpty = Proxy

  annIsEmpty _ = True
  annMerge _ _ = (Proxy, False)
  annF _ = (Proxy, False)

example1 :: Egraph Ex1 (Proxy Ex1)
example1 = executingState eempty do
  a <- einsert (H 3)
  b <- einsert (H 4)
  c <- einsertFix $ Fix (F (Fix $ H 3) (Fix $ G (Fix $ H 4)))
  d <- einsertFix $ Fix (F (Fix $ H 4) (Fix $ H 5))
  eunion a b
  e <- einsert (H 5)
  f <- einsertFree (Free $ G (Pure e))
  eunion e f
  pass

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
  foldr unionAllEqrel e
    $ getMonoidalMap
    $ ifoldMap
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