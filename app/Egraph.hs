{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Egraph
  ( Egraph,
    eempty,
    efind,
    eunion,
    -- for examples: non-propagating union
    eunionInternal,
    elookup,
    einsert,
    einsertFix,
    einsertFree,
    econcretize,
    edebug,
    eannotation,
    ereannotate,
    prettyId,
    EId (..),
    unId,
    Signature (..),
  )
where

import Control.Lens hiding (para)
import Control.Monad.Free (Free (..), iter)
import Data.Fix (Fix)
import Data.Functor.Foldable (cata)
import Data.IntMap.Merge.Strict qualified as IntMapMerge
import Data.IntMap.Monoidal.Strict (getMonoidalIntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.IntSet.Lens qualified as IntSet
import Data.Map.Merge.Strict qualified as MapMerge
import Data.Map.Monoidal (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
-- import Egraph (EId (..), Signature (..), Use (..), unId)
import Lude
import Prettyprinter (Doc, indent, line, list, viaShow, vsep, (<+>))

newtype EId = Id {_unId :: Int}
  deriving (Eq, Ord, Hashable, Show, Read, Generic)

makeWrapped ''EId

instance Bounded EId where
  minBound = Id 0
  maxBound = Id maxBound

makeLenses ''EId

class (Ord (ACSymbol enode), Ord (Symbol enode), Ord (enode EId), Traversable enode) => Signature enode where
  type Symbol enode

  symbolOf :: enode a -> Symbol enode
  default symbolOf :: (Symbol enode ~ enode ()) => enode a -> Symbol enode
  symbolOf = void

  type ACSymbol enode

  acSymbolOf :: enode a -> Maybe (ACSymbol enode)
  default acSymbolOf :: (ACSymbol enode ~ Void) => enode a -> Maybe (ACSymbol enode)
  acSymbolOf _ = Nothing

  arity :: enode a -> Int
  default arity :: (Symbol enode ~ enode ()) => enode a -> Int
  arity = length

  -- Who doesn't love ambiguity?
  arity' :: Proxy enode -> Symbol enode -> Int
  default arity' :: (Symbol enode ~ enode ()) => Proxy enode -> Symbol enode -> Int
  arity' _ = length

  -- arityAC :: Proxy enode -> ACSymbol enode -> Int
  -- default arityAC :: (ACSymbol enode ~ Void) => Proxy enode -> ACSymbol enode -> Int
  -- arityAC _ = absurd

  -- it has to be partial in certain cases...
  reconstruct :: Symbol enode -> [a] -> enode a
  default reconstruct :: (Symbol enode ~ enode ()) => Symbol enode -> [a] -> enode a
  reconstruct s as = s & unsafePartsOf traverse .~ as

  reconstructAC :: ACSymbol enode -> [a] -> enode a
  default reconstructAC :: (ACSymbol enode ~ Void) => ACSymbol enode -> [a] -> enode a
  reconstructAC = absurd

-- reconstruct :: Symbol enode -> [a] -> Maybe (enode a)
-- default reconstruct :: (Symbol enode ~ enode ()) => Symbol enode -> [a] -> Maybe (enode a)
-- reconstruct s as = sequence . (partsOf traverse .~ fmap Just as) . (Nothing <$) $ s

data Use c
  = LeftUse c
  | RightUse c
  deriving (Eq, Ord, Show)

atId :: (At m, Index m ~ Int) => EId -> Lens' m (Maybe (IxValue m))
atId e = at (e ^. unId)

-- Map from EIds to exponent
type Monomial = IntMap Int

grevlex :: Monomial -> Monomial -> Ordering
grevlex t u = comparing cmp t u
  where
    cmp r = (sum r, map negate (degrees r)) -- grevlex order
    vars = sort (IntMap.keys t ++ IntMap.keys u)
    degrees r = [IntMap.findWithDefault 0 x r | x <- vars]

exponentAt :: EId -> Lens' Monomial Int
exponentAt e = atId e . non 0

-- TODO: isPureMon/pureMon/allocId to cleanup code

reduceMon :: Monomial -> (Monomial, Monomial) -> (Bool, Monomial)
reduceMon x (l, r) =
  -- Step 1. Find the maximum k such that x is divisible by l^k
  -- x^i in r1, x^0 in l2 → no constraints on k
  -- x^i in r1, x^j in l2 → k can be at most div i j
  -- x^0 in r1, x^j in l2 → k can be at most 0 (special case of the above)
  -- (technically, the first case is also a special case if we consider the partially ordered set N union infinity)
  -- Q: could be optimized by mergeA (make the third case go to Nothing, to shortcut
  -- when one monomial is not divisible by the other)
  let maxKMap = IntMapMerge.merge IntMapMerge.dropMissing (IntMapMerge.mapMissing \_ _ -> 0) (IntMapMerge.zipWithMatched \_ a b -> div a b) x l
      -- the maximum k is bounded by each of the divisors above... just to say yes,
      -- this is supposed to be the minimum
      maxK = minimumOf folded maxKMap -- if null maxKMap then Nothing else Just $ minimum maxKMap
   in case maxK of
        -- Example of this case: x = x_1 x_2, l = 1
        -- Clearly, x is always divisible by l^k no matter the k
        -- In fact, this case only happens when l is 1.
        -- The rewrite rule 1 → r for some r is a bit ill-behaved,
        -- theoretically we 'should' rewrite x to x r^infinity
        -- But we will just pretend nothing happened and hope this case never occurs
        Nothing -> (False, x)
        Just k ->
          if k == 0
            then (False, x)
            else
              let x_div_l_k = IntMapMerge.merge IntMapMerge.preserveMissing IntMapMerge.dropMissing (IntMapMerge.zipWithMatched \_ a b -> a - k * b) x l
               in (True, filter (> 0) $ IntMap.unionWith (+) x_div_l_k (fmap (* k) r))

reduceMons :: Monomial -> Map Monomial Monomial -> (Bool, Monomial)
reduceMons x ms =
  foldl'
    (\(b, x') m -> first (b ||) (reduceMon x' m))
    (False, x)
    (itoList ms)

createMon :: [EId] -> Monomial
createMon = coerce . getMonoidalIntMap . foldMap (\e -> fromList [(e ^. unId, Sum @Int 1)])

unMon :: Monomial -> [EId]
unMon = ifoldMap (\e k -> replicate k (Id e))

prettyMon :: Monomial -> Doc ann
prettyMon m | null m = "1"
prettyMon m = list . fmap (\(x, i) -> if i == 1 then viaShow x else viaShow x <> "^" <> viaShow i) . itoList $ m

data Egraph f ann
  = EgraphC
  { _egUFRoot :: IntMap EId,
    _egUFBack :: IntMap IntSet,
    _egAnn :: IntMap ann,
    _egTo :: Map (f EId) EId,
    _egBack :: IntMap (Set (Use (f EId))),
    _egNext :: EId,
    _egAC :: Map (ACSymbol f) (Map Monomial Monomial),
    _egBaseEqs :: [(EId, EId)],
    _egMonoEqs :: Seq (ACSymbol f, Monomial, Monomial),
    _egChangedAnn :: IntSet,
    _egBottom :: ann,
    _egMerge :: ann -> ann -> (Bool, ann),
    _egAlg :: f ann -> ann
  }

makeLenses ''Egraph

eempty :: ann -> (ann -> ann -> (Bool, ann)) -> (f ann -> ann) -> Egraph f ann
eempty =
  EgraphC
    IntMap.empty
    IntMap.empty
    IntMap.empty
    Map.empty
    IntMap.empty
    (Id 0)
    Map.empty
    []
    []
    IntSet.empty

efind :: EId -> State (Egraph f ann) EId
efind e = use (egUFRoot . atId e . non e)

-- Internal
euses :: EId -> Lens' (Egraph f ann) (Set (Use (f EId)))
euses e = egBack . atId e . anon Set.empty Set.null

-- Internal
emono :: (Signature f) => ACSymbol f -> Lens' (Egraph f ann) (Map Monomial Monomial)
emono s = egAC . at s . anon Map.empty Map.null

eannotation :: EId -> State (Egraph f ann) ann
eannotation e = use (egAnn . atId e) >>= maybe (use egBottom) pure

elookup :: (Signature f) => f EId -> State (Egraph f ann) (Maybe EId)
elookup f = do
  f' <- traverse efind f
  case acSymbolOf f' of
    Just s -> do
      rws <- use (emono s)
      let reduced = snd (reduceMons (createMon (toList f')) rws)
      case itoList reduced of
        [(x, 1)] -> pure (Just $ Id x)
        _ -> pure Nothing
    Nothing -> use (egTo . at f')

einsertInternal :: (Signature f) => f EId -> State (Egraph f ann) EId
einsertInternal f = do
  f' <- traverse efind f
  use (egTo . at f') >>= \case
    Just i -> pure i
    Nothing -> do
      i <- egNext <<%= (unId +~ 1)
      egTo . at f' ?= i
      euses i . contains (RightUse f') .= True
      for_ f' \j -> euses j . contains (LeftUse f') .= True

      fa <- traverse (efind >=> eannotation) f'
      m <- use egAlg
      egAnn . atId i ?= m fa

      pure i

einsertACInternal :: (Signature f) => ACSymbol f -> f EId -> State (Egraph f ann) EId
einsertACInternal s f = do
  f' <- traverse efind f
  rws <- use (emono s)
  let reduced = snd (reduceMons (createMon (toList f')) rws)
  case itoList reduced of
    [(x, 1)] -> pure (Id x)
    _ -> do
      i <- egNext <<%= (unId +~ 1)
      emono s . at reduced ?= fromList [(i ^. unId, 1)]

      fa <- traverse (efind >=> eannotation) f'
      m <- use egAlg
      egAnn . atId i ?= m fa

      erebuildAC
      pure i

einsert :: (Signature f) => f EId -> State (Egraph f ann) EId
einsert f = case acSymbolOf f of
  Just s -> einsertACInternal s f
  Nothing -> einsertInternal f

einsertFix :: (Signature f) => Fix f -> State (Egraph f ann) EId
einsertFix = cata (sequence >=> einsert)

einsertFree :: (Signature f) => Free f EId -> State (Egraph f ann) EId
einsertFree = iter (sequence >=> einsert) . fmap pure

-- Internal
epropagateBase :: (Signature f) => State (Egraph f ann) ()
epropagateBase =
  use egBaseEqs >>= \case
    [] -> pass
    ((a, b) : rs) -> do
      egBaseEqs .= rs
      void (eunionInternal a b)
      epropagateBase

-- Internal
epropagateAnns :: (Signature f) => State (Egraph f ann) ()
epropagateAnns =
  use egChangedAnn >>= \m -> case IntSet.minView m of
    Nothing -> pass
    Just (i, m') -> do
      egChangedAnn .= m'
      i' <- efind (Id i)
      use (euses i') >>= traverse_ \case
        LeftUse en -> do
          en' <- traverse efind en
          r <- use (egTo . at en')
          whenJust r \r' -> do
            fa <- traverse (efind >=> eannotation) en'
            alg <- use egAlg
            eupdateAnnotation r' (alg fa)
        RightUse _ -> pass

-- Internal
eupdateAnnotation :: EId -> ann -> State (Egraph f ann) ()
eupdateAnnotation e a = do
  oldAnn <- eannotation e
  m <- use egMerge
  let (didChangeAnn, newAnn) = m oldAnn a
  egAnn . atId e ?= newAnn
  -- for later propagation
  when didChangeAnn (egChangedAnn . atId e ?= ())

ereannotate :: (Signature f) => EId -> ann -> State (Egraph f ann) ()
ereannotate e a = eupdateAnnotation e a <* epropagateAnns

-- Internal
erebuildMon :: Monomial -> State (Egraph f ann) Monomial
erebuildMon m = do
  m' <- traverse (\(i, k) -> (,k) <$> efind (Id i)) (itoList m)
  pure (coerce . getMonoidalIntMap . foldMap (\(i, k) -> fromList [(i ^. unId, Sum k)]) $ m')

-- Internal
erebuildAC :: (Signature f) => State (Egraph f ann) ()
erebuildAC = do
  -- empty the existing system... very non-incremental!
  use egAC >>= itraverse_ \s m -> do
    m' <-
      traverse
        (\(l, r) -> (,) <$> erebuildMon l <*> erebuildMon r)
        (itoList m)
    egAC . at s ?= Map.empty
    egMonoEqs <>= fromList (fmap (\(l, r) -> (s, l, r)) m')
  epropagateAC

epropagateAC :: (Signature f) => State (Egraph f ann) ()
epropagateAC =
  use egMonoEqs >>= \eqs -> case uncons eqs of
    Nothing -> pass
    Just ((s, l, r), es) -> do
      egMonoEqs .= es
      cps <- zoom (egAC . at s . anon Map.empty Map.null) (einsertACRW l r)
      -- handle case when l and r are trivial (one entry) by deferring back
      -- to the Egraph
      for_ cps \(l', r') -> case (itoList l', itoList r') of
        ([(x, 1)], [(y, 1)]) -> do
          eunionInternal (Id x) (Id y)
          epropagateBase
          epropagateAnns
        _ -> do
          egMonoEqs <|= (s, l', r')
      epropagateAC

einsertACRW :: Monomial -> Monomial -> State (Map Monomial Monomial) [(Monomial, Monomial)]
einsertACRW l r = do
  reducedL <- get <&> ifoldl' (\l2 acc r2 -> snd $ reduceMon acc (l2, r2)) l
  reducedR <- get <&> ifoldl' (\l2 acc r2 -> snd $ reduceMon acc (l2, r2)) r
  if reducedL /= reducedR
    then case grevlex reducedL reducedR of
      EQ -> pure []
      GT -> extendingState' [] (handleCritical (reducedL, reducedR))
      LT -> extendingState' [] (handleCritical (reducedR, reducedL))
    else pure []

criticalPair :: Monomial -> Monomial -> Maybe Monomial
criticalPair a b =
  let (anyNonTrivial, criticalTerm) =
        IntMapMerge.mergeA
          (IntMapMerge.traverseMissing (\_ ia -> (Any False, ia)))
          (IntMapMerge.traverseMissing (\_ ib -> (Any False, ib)))
          (IntMapMerge.zipWithAMatched (\_ ia ib -> (Any True, max ia ib)))
          a
          b
   in if getAny anyNonTrivial then Just criticalTerm else Nothing

handleCritical :: (Monomial, Monomial) -> State (Map Monomial Monomial, [(Monomial, Monomial)]) ()
handleCritical (l1, r1) = do
  use _1 >>= itraverse_ \l2 r2 -> do
    let (didReduce, r2') = reduceMon r2 (l1, r1)
    when didReduce do
      _1 . at l2 ?= r2'

    let (didReduce2, l2') = reduceMon l2 (l1, r1)
    when didReduce2 do
      _1 . at l2 .= Nothing
    -- The following code will add (l2, r2) to the worklist
    -- since if l2 is reducible by l1, then l2 is a critical term already
    let crit = criticalPair l1 l2
    whenJust crit \crit' -> do
      let (_, crit1) = reduceMon crit' (l1, r1)
      let (_, crit2) = reduceMon crit' (l2, r2)
      _2 <|= (crit1, crit2)
  _1 . at l1 ?= r1

-- Internal
-- remove f(a, b, c) → d, and update the relevant reverse indices. Return d
eremoveRewrite :: Signature f => f EId -> State (Egraph f ann) (Maybe EId)
eremoveRewrite en = use (egTo . at en) >>= traverse \d -> do
  egTo . at en .= Nothing
  euses d . contains (RightUse en) .= False
  for_ en \e -> euses e . contains (LeftUse en) .= False
  pure d

-- Internal
-- add f(a, b, c) → d, and update the relevant reverse indices.
eaddRewrite :: Signature f => f EId -> EId -> State (Egraph f ann) (Maybe EId)
eaddRewrite en d = use (egTo . at en) >>= \clash -> do
  egTo . at en ?= d
  euses d . contains (RightUse en) .= True
  for_ en \e -> euses e . contains (LeftUse en) .= True
  pure clash

-- If we update a → b, then:
-- If we have LeftUse f(a, ...) → d, remove it and replace it with f(b, ...) → d
-- 'Left update'
-- If we have RightUse f(...) → a, remove it and replace it with f(...) → b
-- 'Right update'
-- Left update:
--   f(a, ...) → Nothing
--   remove f(a...) from RightUse(d)
--   remove f(a...) from LeftUse(a)
--   for e in ..., remove f(a...) from LeftUse(e)
--   f(b, ...) → d
--   add f(b...) to RightUse(d)
--   add f(b...) to LeftUse(b)
--   for e in ..., add f(b...) to LeftUse(e)
-- Right update:
--   f(...) → Nothing
--   remove f(...) from RightUse(a)
--   1> for e in ..., remove f(...) from LeftUse(e)
--   f(...) → b
--   add f(...) to RightUse(b)
--   2> for e in ..., add f(...) to LeftUse(e)
-- Note: 1> and 2> together are a no-op

-- Internal
eunionInternal :: (Signature f) => EId -> EId -> State (Egraph f ann) EId
eunionInternal a b = do
  a' <- efind a
  b' <- efind b
  if a' == b'
    then pure a'
    else do
      usesA <- use (euses a')
      usesB <- use (euses b')
      let (newRoot, newChild, _usesRoot, usesChild) =
            if length usesA < length usesB
              then (b', a', usesB, usesA)
              else (a', b', usesA, usesB)

      -- recanonicalizing the UF
      sc <- egUFBack . atId newChild . anon IntSet.empty IntSet.null <<.= IntSet.empty
      egUFRoot . atId newChild ?= newRoot
      forOf_ IntSet.members sc \y -> egUFRoot . at y ?= newRoot
      egUFBack . atId newRoot . anon IntSet.empty IntSet.null <>= one (newChild ^. unId) <> sc

      -- Updating annotations
      oldCAnn <- eannotation newChild
      egAnn . atId newChild .= Nothing
      eupdateAnnotation newRoot oldCAnn

      usesChild' <-
        fromList <$> for (toList usesChild) \case
          LeftUse enOld -> do
            let enNew = fmap (\e -> if e == newChild then newRoot else e) enOld
            rA <- eremoveRewrite enOld
            whenJust rA \s -> do
              rB <- eaddRewrite enNew s
              -- could call recursively...
              whenJust rB \t -> when (s /= t) $ egBaseEqs <|= (s, t)

            -- Handled in bulk by the two lines below **
            -- euses newChild . contains (LeftUse en) .= False
            -- euses newRoot . contains (LeftUse en') .= True

            -- Old code, probably buggy
            -- rA <- use (egTo . at enOld)
            -- rB <- use (egTo . at en)
            -- whenJust rA \s -> do
            --   egTo . at enOld .= Nothing
            --   euses s . contains (RightUse enOld) .= False
            --   egTo . at enNew ?= s
            --   euses s . contains (RightUse enNew) .= True
            --   whenJust rB $ \t -> when (s /= t) $ egBaseEqs <|= (s, t)
            pure (LeftUse enNew)
          RightUse en -> do
            egTo . at en ?= newRoot
            -- Handled in bulk by the two lines below **
            -- euses newChild . contains (RightUse en) .= False
            -- euses newRoot . contains (RightUse en') .= True
            pure (RightUse en)

      -- ** Handled here
      euses newChild .= Set.empty
      euses newRoot <>= usesChild'

      pure newRoot

eunion :: (Signature f) => EId -> EId -> State (Egraph f ann) EId
eunion a b = do
  c <- eunionInternal a b
  epropagateBase
  erebuildAC
  epropagateAnns
  pure c

-- Internal
-- Prepares for concretization by putting all the AC stuff in the normal E-graph
-- since we don't care about the distinction if we are just showing the E-graph
epurifyAC :: (Signature f) => State (Egraph f ann) ()
epurifyAC =
  use egAC
    >>= itraverse_
      ( \s -> itraverse_ \l r -> do
          il <- case itoList l of
             [(x, 1)] -> pure $ Id x
             _ -> einsertInternal (reconstructAC s (unMon l))
          ir <- case itoList r of
             [(x, 1)] -> pure $ Id x
             _ -> einsertInternal (reconstructAC s (unMon r))
          _ <- eunion il ir
          pass
      )

econcretize :: (Signature f) => Egraph f ann -> [(EId, [f EId], ann)]
econcretize eg =
  let eg' = executingState eg epurifyAC
   in toList $ imap
        ( \a us ->
            ( Id a,
              mapMaybe
                ( \case
                    LeftUse _ -> Nothing
                    RightUse en -> Just en
                )
                (toList us),
              fromMaybe (eg' ^. egBottom) (eg' ^. egAnn . at a)
            )
        )
        (eg' ^. egBack)

edebug :: (ACSymbol f -> Doc a) -> (ann -> Doc a) -> (f EId -> Doc a) -> Egraph f ann -> Doc a
edebug showACSym showAnn showNode eg =
  "Egraph with"
    <+> prettyId (eg ^. egNext)
    <+> "ids"
      <> line
      <> indent
        2
        ( vsep
            [ "egUFRoot"
                <+> indent
                  2
                  (vsep (fmap (\(i, j) -> viaShow i <+> "→" <+> prettyId j) (itoList (eg ^. egUFRoot)))),
              "egUFBack"
                <+> indent
                  2
                  (vsep (fmap (\(j, is) -> viaShow j <+> "←" <+> list (fmap viaShow (toListOf IntSet.members is))) (itoList (eg ^. egUFBack)))),
              "egAnn"
                <+> indent
                  2
                  (vsep (fmap (\(i, a) -> viaShow i <+> "has annotation" <+> showAnn a) (itoList (eg ^. egAnn)))),
              "egTo"
                <+> indent
                  2
                  (vsep ((\(lhs, rhs) -> showNode lhs <+> "→" <+> prettyId rhs) <$> itoList (eg ^. egTo))),
              "egBack"
                <+> indent
                  2
                  ( vsep
                      ( fmap
                          ( \(i, us) ->
                              viaShow i
                                <+> "is used in:"
                                  <> line
                                  <> indent
                                    2
                                    ( vsep
                                        ( fmap
                                            ( \case
                                                LeftUse en -> "some rule" <+> showNode en <+> "→" <+> "?"
                                                RightUse en -> "the rule" <+> showNode en <+> "→" <+> viaShow i
                                            )
                                            (toList us)
                                        )
                                    )
                          )
                          (itoList (eg ^. egBack))
                      )
                  ),
              "egAC"
                <+> indent
                  2
                  ( vsep
                      ( fmap
                          ( \(s, acs) ->
                              "the symbol"
                                <+> showACSym s
                                <+> "has AC-system:"
                                  <> line
                                  <> vsep
                                    ( fmap
                                        (\(l, r) -> prettyMon l <+> "→" <+> prettyMon r)
                                        (itoList acs)
                                    )
                          )
                          (itoList (eg ^. egAC))
                      )
                  ),
              "egBaseEqs"
                <+> indent 2 (vsep (fmap (\(a, b) -> prettyId a <+> "≟" <+> prettyId b) (eg ^. egBaseEqs))),
              "egMonoEqs"
                <+> indent 2 (vsep (fmap (\(s, a, b) -> showACSym s <> ":" <+> prettyMon a <+> "≟" <+> prettyMon b) (toList (eg ^. egMonoEqs)))),
              "egChangedAnn"
                <+> indent 2 (list (fmap viaShow (toListOf IntSet.members (eg ^. egChangedAnn))))
            ]
        )

prettyId :: EId -> Doc ann
prettyId i = viaShow (i ^. unId)