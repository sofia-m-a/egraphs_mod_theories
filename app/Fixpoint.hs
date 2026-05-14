{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fixpoint where

import Control.Lens hiding (para)
import Control.Monad.Free (Free (..))
import Data.Foldable (foldrM)
import Data.IntMap qualified as IntMap
import Data.Map.Merge.Lazy (mergeA, traverseMissing, zipWithAMatched)
import Data.Map.Monoidal (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Egraph (EId (..), Ex1 (..), Signature (..), Use (..), unId)
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

instance CSL () where
  type Delta () = ()
  cslBottom = ()
  cslIsBottom _ = True
  cslMerge _ _ = ((), ())
  cslDeltaIsZero _ _ = True

instance (CSL a, CSL b) => CSL (a, b) where
  type Delta (a, b) = (Delta a, Delta b)
  cslBottom = (cslBottom, cslBottom)
  cslIsBottom (a, b) = cslIsBottom a && cslIsBottom b
  cslMerge (a, b) (c, d) =
    let (x1, r1) = cslMerge a c
        (x2, r2) = cslMerge b d
     in ((x1, x2), (r1, r2))
  cslDeltaIsZero _ (a, b) = cslDeltaIsZero (Proxy @a) a && cslDeltaIsZero (Proxy @b) b

-- Pointwise CSL
instance (Ord b, CSL d) => CSL (Map b d) where
  -- The right monoid instance instance comes from MonoidalMap
  -- (pointwise, using the monoid on Delta)
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

-- I should probably do a newtype for the case Delta a = a ....
instance (Ord b) => CSL (Set b) where
  type Delta (Set b) = Set b
  cslBottom = Set.empty
  cslIsBottom = Set.null
  cslMerge a b = (Set.difference a b, Set.union a b)
  cslDeltaIsZero _ = Set.null

-- non-lazy union find
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

type DeltaF d = d -> (Delta d, d)

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

class (CSL s) => WithEqrel v s where
  equate :: (v, v) -> s -> (Delta s, s)

instance (Ord v, CSL b) => WithEqrel v (Map v b) where
  equate (a, b) m = case m ^. at a of
    Nothing -> (mempty, m)
    Just va ->
      let vb = fromMaybe cslBottom (m ^. at b)
          (d, v) = cslMerge va vb
       in (fromList [(b, d)], m & at a .~ Nothing & at b ?~ v)

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
updateAnnotation v a ae =
  let v' = findEqrel (ae ^. eqrel) v
      (d, ae') = ae & annotationOf v' %%~ cslMerge a
   in ((mempty, [(v', d)]), ae')

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
     in -- start with e2's annotations, see what has changed
        executingState (mempty, AnnEqrelC newE (e2 ^. annotations)) do
          for_ changedE \(a, c) -> do
            oldA <- _2 . annotationOf a <<.= cslBottom
            d <- _2 %%= updateAnnotation c oldA
            _1 <>= d
          ifor_ (e1 ^. annotations) \a ann -> do
            d <- _2 %%= updateAnnotation a ann
            _1 <>= d
  cslDeltaIsZero _ (d, m) = cslDeltaIsZero (Proxy @(Eqrel v)) d && cslDeltaIsZero (Proxy @(Map v a)) m

-- merge over empty, f(empty), ... f^i(empty), etc
cslFixpoint :: forall a. (CSL a) => DeltaF a -> a
cslFixpoint f = go cslBottom
  where
    go a = let (d, a') = f a in if cslDeltaIsZero (Proxy @a) d then a' else go a'

cslFixpointM :: forall a m. (Monad m, CSL a) => (a -> m (Delta a, a)) -> m a
cslFixpointM f = go cslBottom
  where
    go a = do
      (d, a') <- f a
      if cslDeltaIsZero (Proxy @a) d then pure a' else go a'

-- Convention: we left-associate
-- f(a, b, c) = ((CurrySymbol f `CurryApp` a) `CurryApp` b) `CurryApp` c
data Currified f a
  = CurryApp a a
  | CurrySymbol (Symbol f)
  deriving (Functor, Foldable, Traversable)

currify :: (Signature f) => f a -> Free (Currified f) a
currify f = foldl' (\a b -> Free (CurryApp a (Pure b))) (Free $ CurrySymbol $ symbolOf f) (toList f)

deriving instance (Eq a, Signature f) => Eq (Currified f a)

deriving instance (Ord a, Signature f) => Ord (Currified f a)

instance (Signature f) => Signature (Currified f) where
  type Symbol (Currified f) = Maybe (Symbol f)
  symbolOf :: (Signature f) => Currified f a -> Symbol (Currified f)
  symbolOf (CurryApp _ _) = Nothing
  symbolOf (CurrySymbol s) = Just s

  arity :: (Signature f) => Currified f a -> Int
  arity (CurryApp _ _) = 2
  arity (CurrySymbol s) = arity' (Proxy @f) s

  arity' _ Nothing = 2
  arity' _ (Just s) = arity' (Proxy @f) s

  reconstruct (Just s) [] = CurrySymbol s
  reconstruct Nothing [a, b] = CurryApp a b
  reconstruct _ _ = error "arity issue"

-- given an analysis of shape f a -> a,
-- we want to build one of shape Currified f (CurryAnalysis a) -> CurryAnalysis a
-- we can't just use a as the carrier, as having seen e.g. ((f a) b), we haven't yet
-- seen a c to 'fire' the corresponding analysis on f(a, b, c)

-- unfortunately, it seems rather hard to encode into the type system the
-- invariant that the analysis is always 'arity-correct'. So the instances are not
-- entirely lawful. Really, we ought to have a type of sorts and have enodes be
-- sort-indexed, and then we can define type Sort (Currified f a) etc...
data CurryAnalysis s a
  = CurryDone a
  | CurryPartial s Int [a]
  deriving (Functor, Foldable, Traversable)

instance (Semigroup a) => Semigroup (CurryAnalysis f a) where
  CurryDone a <> CurryDone b = CurryDone (a <> b)
  CurryPartial s i as <> CurryPartial _ _ bs = CurryPartial s i (zipWith (<>) as bs)
  _ <> _ = error "arity mismatch"

instance (Monoid a) => Monoid (CurryAnalysis f a) where
  mempty = CurryDone mempty

instance (CSL a) => CSL (CurryAnalysis s a) where
  type Delta (CurryAnalysis s a) = CurryAnalysis () (Delta a)
  cslBottom = CurryDone cslBottom
  cslIsBottom (CurryDone a) = cslIsBottom a
  cslIsBottom (CurryPartial _ _ as) = all cslIsBottom as

  cslMerge (CurryDone a) (CurryDone b) = let (d, c) = cslMerge a b in (CurryDone d, CurryDone c)
  cslMerge (CurryPartial s i as) (CurryPartial _ _ bs) =
    let (ds, cs) = unzip (zipWith cslMerge as bs) in (CurryPartial () i ds, CurryPartial s i cs)
  cslMerge _ _ = error "arity mismatch"

  cslDeltaIsZero _ (CurryDone d) = cslDeltaIsZero (Proxy @a) d
  cslDeltaIsZero _ (CurryPartial _ _ as) = all (cslDeltaIsZero (Proxy @a)) as

curryAnalysis :: forall f a. (Signature f, CSL a) => (f a -> a) -> Currified f (CurryAnalysis (Symbol f) a) -> CurryAnalysis (Symbol f) a
curryAnalysis ann (CurrySymbol s) = curryCheckComplete ann (CurryPartial s 0 [])
curryAnalysis ann (CurryApp a b) = case a of
  CurryDone _ -> error "arity issue"
  CurryPartial s n as -> case b of
    CurryPartial {} -> a
    CurryDone b' -> curryCheckComplete ann (CurryPartial s (n - 1) (b' : as))

curryCheckComplete :: (Signature enode) => (enode a -> a) -> CurryAnalysis (Symbol enode) a -> CurryAnalysis (Symbol enode) a
curryCheckComplete ann (CurryPartial s 0 as) = CurryDone (ann (reconstruct s (reverse as)))
curryCheckComplete _ c = c

data Egraph f ann
  = Egraph
  { _ann :: AnnEqrel EId ann,
    _eto :: Map (f EId) EId,
    _eback :: IntMap (Set (Use (f EId))),
    _enext :: EId
  }

deriving instance (Show ann, Show (f EId)) => Show (Egraph f ann)

makeLenses ''Egraph

efind :: EId -> State (Egraph f ann) EId
efind e = use (ann . eqrel) <&> flip findEqrel e

elookup :: (Signature f) => f EId -> State (Egraph f ann) (Maybe EId)
elookup f = do
  f' <- traverse efind f
  use (eto . at f')

euses :: EId -> Lens' (Egraph f ann) (Set (Use (f EId)))
euses e = eback . at (e ^. unId) . anon Set.empty Set.null

einsert' :: (Signature f) => f EId -> State (Egraph f ann) EId
einsert' f = do
  f' <- traverse efind f
  use (eto . at f') >>= \case
    Just i -> pure i
    Nothing -> do
      i <- enext <<%= (unId +~ 1)
      eto . at f' ?= i
      euses i . contains (RightUse f') .= True
      for_ f' \j -> euses j . contains (LeftUse f') .= True
      pure i

einsert :: (Signature f, CSL ann) => (f ann -> ann) -> f EId -> State (Egraph f ann) EId
einsert anno f = do
  i <- einsert' f
  fa <- traverse (efind >=> \e -> use (ann . annotationOf e)) f
  let newA = anno fa
  eannotate anno i newA
  pure i

-- internal: does only union and merging annotations,
-- no application of f ann -> ann algebra
-- keeps state fields to track deltas
-- needs the deltas to also be looped...
eunion' :: (Signature f, CSL ann) => EId -> EId -> State (Egraph f ann, [(EId, EId)], MonoidalMap EId (Delta ann)) EId
eunion' a b = do
  a' <- zoom _1 (efind a)
  b' <- zoom _1 (efind b)
  if a' == b'
    then pure a'
    else do
      usesA <- use (_1 . euses a')
      usesB <- use (_1 . euses b')
      let (newRoot, newChild, usesRoot, usesChild) =
            if length usesA < length usesB
              then (b', a', usesB, usesA)
              else (a', b', usesA, usesB)
      _1 . ann . eqrel %= unionEqrel' newChild newRoot
      -- d': annotations delta
      d' <- _1 . ann . annotations %%= equate (newChild, newRoot)
      -- we may as well recanonicalize the uses

      -- The invariant is that LeftUses in the uses list
      -- can be noncanonical, but the forward index (eto) is always canonical.
      -- Therefore, when merging a and b, we need to look for things
      -- f(c1, ..., a, ...) and f(c1, ..., b, ...) where c1... are canonical
      -- RightUses in the uses list are canonical!
      usesChild' <-
        fromList <$> for (toList usesChild) \case
          LeftUse en -> do
            -- canonicalize everything but newChild
            enOld <- traverse (\e -> if e == newChild then pure e else zoom _1 (efind e)) en
            let enNew = fmap (\e -> if e == newChild then newRoot else e) enOld
            rA <- use (_1 . eto . at enOld)
            rB <- use (_1 . eto . at en)
            whenJust rA \s -> do
              _1 . eto . at enOld .= Nothing
              _1 . euses s . contains (RightUse enOld) .= False
              _1 . eto . at enNew ?= s
              _1 . euses s . contains (RightUse enNew) .= True
              -- could call recursively...
              whenJust rB $ \t -> when (s /= t) $ _2 <|= (s, t)
            pure (LeftUse enNew)
          RightUse en -> do
            en' <- traverse (zoom _1 . efind) en
            -- this handles the case where we update a cycle: f(...a...) -> a
            _1 . eto . at en .= Nothing
            _1 . eto . at en' ?= newRoot
            pure (RightUse en')
      _1 . euses newChild .= Set.empty
      _1 . euses newRoot .= usesChild' <> usesRoot
      _2 <|= (newChild, newRoot)
      _3 <>= d'
      pure newRoot

eunion :: (Signature f, CSL ann) => (f ann -> ann) -> EId -> EId -> State (Egraph f ann) EId
eunion anno a b = do
  eg <- get
  let (out, (eg', us, ds)) = runState (eunion' a b) (eg, [], mempty)
  put eg'
  epropagate anno us ds
  pure out

eannotate :: (Signature f, CSL ann) => (f ann -> ann) -> EId -> ann -> State (Egraph f ann) ()
eannotate anno v a = do
  v' <- efind v
  d <- ann . annotationOf v %%= cslMerge a
  epropagate anno [] (fromList [(v', d)])

epropagate :: (CSL ann, Signature f) => (f ann -> ann) -> [(EId, EId)] -> MonoidalMap EId (Delta ann) -> StateT (Egraph f ann) Identity ()
epropagate anno us ds = do
  eg <- get
  put (evalState go (eg, us, ds))
  where
    go = do
      us <- use _2
      case us of
        ((c, d) : us') -> do
          _2 .= us'
          _ <- eunion' c d
          go
        [] -> do
          m <- use (_3 . _Wrapped')
          case Map.minViewWithKey m of
            Nothing -> use _1
            (Just ((i, _), m')) -> do
              _3 . _Wrapped' .= m'
              -- EId i has been updated with delta da, which we won't actually
              -- use... unless we had an incremental thing
              -- df :: f (Delta a) × a -> a
              i' <- zoom _1 (efind i)
              use (_1 . euses i') >>= traverse_ \case
                LeftUse en -> do
                  en' <- traverse (zoom _1 . efind) en
                  fa <- traverse (\e -> use (_1 . ann . annotationOf e)) en'
                  let newA = anno fa
                  r <- use (_1 . eto . at en')
                  whenJust r \r' -> do
                    d <- _1 . ann . annotationOf r' %%= cslMerge newA
                    _3 <>= fromList [(r', d)]
                RightUse _ -> pass
              go

instance CSL Int where
  type Delta Int = Sum Int
  cslBottom = 0
  cslMerge a b = let z = max a b in (Sum (z - min a b), z)
  cslIsBottom = (0 ==)
  cslDeltaIsZero _ = (0 ==)

example1 :: Egraph Ex1 Int
example1 = executingState (Egraph cslBottom Map.empty IntMap.empty (Id 0)) do
  h1 <- einsert example1Alg (H 1)
  h2 <- einsert example1Alg (H 2)
  _ <- eunion example1Alg h1 h2
  f1 <- einsert example1Alg (F h1 h2)
  f2 <- einsert example1Alg (F h1 h1)
  eannotate example1Alg f2 5
  pass

example1Alg :: Ex1 Int -> Int
example1Alg (H i) = i
example1Alg (F i j) = i + j
example1Alg (G z) = -z

-- Old stuff, before I gave up on thinking too hard about semilattices
-- with a monoidal action

-- -- Normalized rewriting system:
-- -- instead of taking an ordinary fixpoint, take a stateful fixpoint where
-- -- we keep around the 'latest' system, for efficiency
-- data NormSystem f v
--   = NormSystemC
--   { _rws :: Map (f v) v
--   }
--   deriving (Show)

-- makeLenses ''NormSystem

-- -- the general idea is if we have Delta a act on a,
-- -- and a acts on b, then Delta a acts on (a semidirect b)
-- -- but, we can code this more efficiently in concrete cases
-- --
-- -- in this case, due to a lack of backward indices, this is still not entirely incremental
-- normstep :: (Functor f, Ord (f v), Ord v) => ([(v, v)], Eqrel v, NormSystem f v) -> ([(v, v)], Eqrel v, NormSystem f v)
-- normstep (ds, e, s) = executingState (ds, e, NormSystemC Map.empty) do
--   ifor_ (s ^. rws) \fi i -> do
--     e' <- use _2
--     let fi' = fmap (findEqrel e') fi
--     let i' = findEqrel e' i
--     clash <- _3 . rws . at fi' <<?= i'
--     whenJust clash \c -> _1 <|= (i', c)

-- data UsageKind = LHS | RHS deriving (Eq, Ord, Show)

-- type IncEgraph f ann = AnnEqrel EId (Set (UsageKind, f EId), ann)

-- -- it's all coming together .jpg
-- incnormstep ::
--   forall f ann.
--   (Signature f, CSL ann) =>
--   (f ann -> ann) ->
--   (Delta (IncEgraph f ann), IncEgraph f ann, NormSystem f EId) ->
--   (Delta (IncEgraph f ann), IncEgraph f ann, NormSystem f EId)
-- incnormstep ann = execState do
--   delta <- _1 <<.= mempty
--   -- Delta (IncEgraph f ann) = ([(EId, EId)], MonoidalMap EId (Set (UsageKind, f EId), ann))
--   -- AnnEqrel handles a good deal of the propagation automatically.
--   -- We need to manually handle
--   -- 1. Updating the NormSystem
--   for_ (delta ^. _1) \(a, b) -> do
--     -- a was merged into b
--     use (_2 . annotationOf a . _1) >>= traverse_ \(k, en) -> case k of
--       LHS -> do
--         -- f(...a...) -> c
--         let en' = fmap (\e -> if e == a then b else e) en

--         rA <- use (_3 . rws . at en)
--         rB <- use (_3 . rws . at en')
--         whenJust rA \s -> do
--           _3 . rws . at en .= Nothing
--           _2 . annotationOf s . _1 . contains (RHS, en) .= False
--           _3 . rws . at en' ?= s
--           _2 . annotationOf s . _1 . contains (RHS, en') .= True
--           whenJust rB \t -> do
--             d <- _2 . eqrel %%= unionEqrelD s t
--             _1 . _1 <>= d
--       RHS -> _3 . rws . at en ?= b
--   -- 2. Updating the annotations
--   for_ (delta ^. _1) \(_, b) -> do
--     -- a and b merged annotations; that means we need to update the annotation for
--     -- any f(...b...) -> c
--     use (_2 . annotationOf b . _1) >>= traverse_ \(k, en) -> case k of
--       LHS -> do
--         fa <- traverse (\e -> use (_2 . annotationOf e . _2)) en
--         r <- use (_3 . rws . at en)
--         whenJust r \r' -> do
--           d <- _2 . annotationOf r' . _2 %%= cslMerge (ann fa)
--           -- yuck
--           _1 . _2 . at r' . anon mempty (cslDeltaIsZero (Proxy @(Set (UsageKind, f EId), ann))) . _2 <>= d
--       -- If something -> b, the propagation of annotations from the something->a
--       -- are already handled when merging a and b
--       RHS -> pass
--   ifor_ (delta ^. _2) \a (deltaUses, deltaAnn) -> do
--     for_ deltaUses \(k, en) -> case k of
--       -- 1. If f(...a...) is added to deltaUses, we need to update f(...a...) -> b
--       LHS -> _
--       RHS -> _
--     pass
--   where
--     reannotate :: f EId -> State (Delta (IncEgraph f ann), IncEgraph f ann, NormSystem f EId) ()
--     reannotate en = do
--       en' <- normalize en
--       fa <- traverse (\e -> use (_2 . annotationOf e . _2)) en
--       let newAnn = ann fa
--       r <- use (_3 . rws . at en')
--       whenJust r \r' -> do
--         d <- _2 . annotationOf r' . _2 %%= cslMerge newAnn
--         -- yuck
--         _1 . _2 . at r' . anon mempty (cslDeltaIsZero (Proxy @(Set (UsageKind, f EId), ann))) . _2 <>= d
--     normalize en = traverse (\e -> use (_2 . eqrel) <&> _) en

-- -- Ought to be an instance of cslFixpointM with StateT over Writer, but I think it
-- -- is probably better to be explicit
-- ccloop :: forall f ann. (Signature f, CSL ann) => (f ann -> ann) -> (IncEgraph f ann, NormSystem f EId) -> (IncEgraph f ann, NormSystem f EId)
-- ccloop ann (x, y) = ccloop' ann (mempty, x, y)

-- ccloop' :: forall f ann. (Signature f, CSL ann) => (f ann -> ann) -> (Delta (IncEgraph f ann), IncEgraph f ann, NormSystem f EId) -> (IncEgraph f ann, NormSystem f EId)
-- ccloop' ann = go
--   where
--     go (prevDelta, eg, rw) = case incnormstep ann (prevDelta, eg, rw) of
--       (newDelta, eg', rw') ->
--         if cslDeltaIsZero (Proxy @(IncEgraph f ann)) newDelta
--           then (eg', rw')
--           else go (newDelta, eg', rw')

-- congruentMerge :: (Functor f, Ord v, Ord (f v), Monoid a) => Map (f v) a -> Eqrel v -> Map (f v) a
-- congruentMerge m e = getMonoidalMap $ ifoldMap (\fi a -> [(fmap (findEqrel e) fi, a)]) m

-- -- nondeterministic rewrite system, interpretation over eqrels
-- rwSystem :: (Functor f, Ord v, Ord (f v)) => Map (f v) [v] -> DeltaF (Eqrel v)
-- rwSystem m e = foldrM unionAllEqrelD e $ congruentMerge m e

-- detrwSystem :: (Functor f, Ord v, Ord (f v)) => Map (f v) v -> DeltaF (Eqrel v)
-- detrwSystem m e = foldrM unionAllEqrelD e $ congruentMerge (fmap one m) e

-- -- magic! ...ish
-- congruenceClosure :: (Functor f, Ord (f Int)) => Map (f Int) [Int] -> Eqrel Int
-- congruenceClosure m = cslFixpoint (rwSystem m)

-- -- CSL cslMerge is a bit awkward to compose, here's a helper
-- data DeltaM a = DeltaM (Delta a) a

-- instance (CSL a) => Semigroup (DeltaM a) where
--   DeltaM d1 a1 <> DeltaM d2 a2 = let (d, a) = cslMerge a1 a2 in DeltaM (d1 <> d2 <> d) a

-- instance (CSL a) => Monoid (DeltaM a) where
--   mempty = DeltaM mempty cslBottom

-- -- the first two arguments are morally f (Int, a) -> (Int, a)
-- -- but this is only finitely presentable via a map on the Int part
-- annRwSystem :: (CSL a, Functor f, Ord v, Ord (f v)) => Map (f v) v -> (f a -> a) -> DeltaF (AnnEqrel v a)
-- annRwSystem m f ae =
--   foldrM
--     -- is throwing the delta away here ok?
--     (\(is, DeltaM _ a) -> unionAllAnnEqrelD (is, a))
--     ae
--     $ congruentMerge
--       (imap (\fi i -> ([i], DeltaM mempty (upd fi) <> DeltaM mempty (ae ^. annotationOf i))) m)
--       (ae ^. eqrel)
--   where
--     upd = f . fmap (\i -> ae ^. annotationOf i)

-- annotatedCongruenceClosure :: (CSL a, Functor f, Ord v, Ord (f v)) => Map (f v) v -> (f a -> a) -> AnnEqrel v a
-- annotatedCongruenceClosure m f = cslFixpoint (annRwSystem m f)

-- -- requirements to make a more efficient version [mostly done]
-- -- 1. Eqrel should be specialized to EqrelInt with IntMaps.
-- -- 2. The same with the other uses for Map/MonoidalMap
-- -- 3. We need a 'uses' map, which is a kind of E-analysis in this hopelessly
-- --   general framework
-- -- 4. Binarization
