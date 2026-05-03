module Egraphattempts where

data Eclass enode = EclassC
  { _classNodes :: Set (enode EId),
    _classParents :: Seq (enode EId, EId)
  }

instance (Signature enode) => Semigroup (Eclass enode) where
  EclassC a b <> EclassC c d = EclassC (a <> c) (b <> d)

instance (Signature enode) => Monoid (Eclass enode) where
  mempty = EclassC mempty mempty

emptyClass :: Eclass enode
emptyClass = EclassC Set.empty mempty

isEmptyClass :: Eclass enode -> Bool
isEmptyClass (EclassC ns ps) = Set.null ns && null ps

makeLenses ''Eclass

data Egraph enode = EgraphC
  { _enodes :: TermTrie EId (Map (Symbol enode) EId),
    _eclasses :: Map EId (Eclass enode),
    _unionFind :: UF,
    _workList :: [Merge]
  }

makeLenses ''Egraph

instance (Signature enode) => Semigroup (Egraph enode) where
  a <> b =
    EgraphC
      (alignWith (mergeThese (<>)) (a ^. enodes) (b ^. enodes))
      (a ^. eclasses <> b ^. eclasses)
      (a ^. unionFind <> b ^. unionFind)
      (a ^. workList <> b ^. workList)

instance (Signature enode) => Monoid (Egraph enode) where
  mempty :: (Signature enode) => Egraph enode
  mempty = EgraphC emptyTT mempty mempty mempty

-- Note: these instances can change the E-class of an E-node,
-- but they don't imply any merging behaviour.
type instance Index (Egraph enode) = enode EId

type instance IxValue (Egraph enode) = EId

instance (Signature enode) => Ixed (Egraph enode)

instance (Signature enode) => At (Egraph enode) where
  at :: enode EId -> Lens' (Egraph enode) (Maybe EId)
  at e = enodes . at (toList e) . anon Map.empty Map.null . at (symbolOf e)

ecanonicalize :: (Signature enode) => enode EId -> Egraph enode -> (enode EId, Egraph enode)
ecanonicalize term egraph = usingState egraph $ traverse (state . efind) term

einsert :: (Signature enode) => enode EId -> Egraph enode -> (EId, Egraph enode)
einsert term' egraph = usingState egraph do
  term <- traverse (state . efind) term'
  use (at term) >>= \case
    Nothing -> do
      eid <- state (unionFind %%~ extend)
      at term ?= eid
      eclasses . at eid . anon emptyClass isEmptyClass . classNodes . at term ?= ()
      for_ term (\e -> eclasses . at e . _Just . classParents <|= (term, eid))
      pure eid
    Just eid -> pure eid

emerge :: (Signature enode) => EId -> EId -> Egraph enode -> (EId, Egraph enode)
emerge a b egraph =
  let ((eid, mr), egraph') = egraph & unionFind %%~ union' a b
   in (eid, egraph' & workList <|~ mr & eclasses %~ mergeEclasses eid)
  where
    mergeEclasses eid = execState do
      a' <- at a <<.= Nothing
      b' <- at b <<.= Nothing
      at eid ?= (maybeToMonoid a' <> maybeToMonoid b')

isCanonical :: Egraph enode -> Bool
isCanonical = view (workList . to null)

esize :: Egraph enode -> Int
esize = view (enodes . to length)

efind :: EId -> Egraph enode -> (EId, Egraph enode)
efind eid egraph = egraph & unionFind %%~ ufind eid

-- TODO: tricky logic
erebuild :: (Signature enode) => Egraph enode -> Egraph enode
erebuild egraph = executingState egraph go
  where
    go = do
      preuse (workList . _Cons) >>= \case
        Nothing -> pass
        Just (MergeC m, ms) -> do
          workList .= ms
          use (eclasses . at m)
            >>= flip
              whenJust
              ( view classParents >>> traverse_ \(oldNode, oldId) -> do
                  newNode <- state (ecanonicalize oldNode)
                  oldId' <- state (efind oldId)
                  currentId <- use (at newNode)
                  at oldNode .= Nothing
                  at newNode ?= fromMaybe oldId currentId
                  case currentId of
                    Just cid | cid /= oldId' -> void (state (emerge cid oldId'))
                    _ -> pass
              )
          go

-- f(...a...) → b
-- a ~> c
-- f(...c...) → b        |  no change
-- f(...c...) → d, b → d |  no change?
-- f(...c...) → d, else  |  merge b d

-- b ← f(c) ← f(a) → b
-- d ← f(c) ← f(a) → b → d


data UFInfo
  = UFInfoC
  { _ufRoot :: EId,
    _ufNext :: EId
  }

makeLenses ''UFInfo

data ENInfo
  = ENInfoC
  { _enRoot :: EId,
    _enNext1 :: EId,
    _enNext2 :: EId
  }

makeLenses ''ENInfo

data TermEq = (EId, EId) :-> EId

data Egraph2 = Egraph2C
  { -- Part 1: union-find with cyclic siblings
    _uf :: IntMap UFInfo,
    _ufSize' :: IntMap Int,
    _ecUses :: IntMap (NonEmpty TermEq),
    _map2 :: Map (EId, EId) EId
  }

makeLenses ''Egraph2

-- Note 1: 'proof-producing congruence closure' suggests doing eager path
-- compression, which makes find no longer stateful

efind2 :: EId -> Lens' Egraph2 UFInfo
efind2 e = uf . at (e ^. unId) . anon (UFInfoC e e) (\(UFInfoC a b) -> a == b)

eroot2 :: EId -> Lens' Egraph2 EId
eroot2 e = efind2 e . ufRoot

esize2 :: EId -> Lens' Egraph2 Int
esize2 e = ufSize' . at (e ^. unId) . anon 1 (== 1)

-- set a = b
eunion2 :: EId -> EId -> Egraph2 -> (EId, Egraph2)
eunion2 a b eg =
  let eg' = epropagate [(a, b)] eg in (eg' ^. eroot2 b, eg')

esiblings :: EId -> Egraph2 -> [EId]
esiblings e eg = go e e
  where
    go a b =
      let UFInfoC _ nx = eg ^. efind2 b
       in if nx == a then [] else b : go a nx

neIso :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
neIso =
  iso
    ( \case
        [] -> Nothing
        (a : as) -> Just (a :| as)
    )
    ( \case
        Nothing -> []
        Just (a :| as) -> a : as
    )

epropagate :: [(EId, EId)] -> Egraph2 -> Egraph2
epropagate [] = id
epropagate ((a, b) : es) = \eg ->
  let (newRoot, newChild, newSize) =
        let (a', b') = (eg ^. eroot2 a, eg ^. eroot2 b)
            (sa, sb) = (eg ^. esize2 a', eg ^. esize2 b')
         in if sa > sb then (a', b', sa + sb) else (b', a', sa + sb)
   in executingState eg do
        -- reroot the smaller class
        get
          >>= traverse_
            (\sib -> efind2 sib . ufRoot .= newRoot)
          . esiblings newChild
        -- update size info
        ufSize' . at (newChild ^. unId) .= Nothing
        ufSize' . at (newRoot ^. unId) ?= newSize
        -- extend sibling pointers
        efind2 newChild . ufNext .= newRoot
        efind2 newRoot . ufNext .= newChild
        -- update pi indexes
        nxs <-
          use (ecUses . at (newChild ^. unId) . from neIso)
            >>= traverse
              ( \((c1, c2) :-> c) -> do
                  c1' <- use (eroot2 c1)
                  c2' <- use (eroot2 c2)
                  use (map2 . at (c1', c2')) >>= \case
                    Just d | c /= d -> do
                      pure (Just (c, d))
                    _ -> do
                      map2 . at (c1', c2') ?= c
                      -- question: does this list ever contain duplicates?
                      -- if so, that would slow things down...
                      ecUses . at (newRoot ^. unId) . from neIso <|= ((c1', c2') :-> c)
                      pure Nothing
              )
        -- aka
        -- ecUses . at (newChild ^. unId) . anon [] null .= []
        ecUses . at (newChild ^. unId) .= Nothing
        modify (epropagate ((nxs >>= toList) ++ es))

-- Making this better:
-- we could use the same trick as for the union find to store the use lists, that is
-- make ecUses map to ENInfo = ENInfoC { _enRoot :: EId, enext1 :: EId, enext2 :: EId }
-- where if ecUses (a, b) = Just (ENInfoC r b' a'),
-- then ecUses (a, b') = Just (ENInfoC r ... ...)
-- and ecUses (a', b) = Just (ENInfoC r ... ...)
-- I'm not sure this is more clear, but maybe it will lead to some other simplification

-- That gives us the following:
-- uf: Map E → (root: E, sib: E)
-- ufSizes : Map E → (size : Nat) (only maps roots! but we could map all without changing asymptotics)
-- enUseList : Map E → (head: TermEq, next1: E, next2: E) (only maps (a subset of) roots)
-- map2 : Map (E, E) → E

-- now, we can distinguish in the types E (non-canonical E-ids) and F (canonical EIds)
-- and we can merge ufSizes, enUseList, and possible map2?
-- uf: Map E → (root: F, sib: E)
-- en: Map E → (size : Nat, head : Maybe (TermEq, next1: E, next2: E), pairMap : Map E → E)
-- then we can also integrate the names of symbols in another map in enodes

-- The TermEq is partly redundant, since we expect en[e].head ~ (e, a) -> c or (a, e) -> c
-- so we split into derivatives 'TermEq1' and 'TermEq2'

data IsCanon = Canon | NonCanon

newtype EId3 (c :: IsCanon) = EId3C {_unEId3 :: Int}
  deriving (Eq)

makeLenses ''EId3

type IdE = EId3 'NonCanon

type IdF = EId3 'Canon

assertCanon :: IdE -> IdF
assertCanon (EId3C e) = EId3C e

forgetCanon :: IdF -> IdE
forgetCanon (EId3C e) = EId3C e

data UF3Info = UF3InfoC
  { _uf3Root :: IdF,
    _uf3NextSib :: IdE
  }

makeLenses ''UF3Info

data EN3Uses c a
  = Use1 a IdF
  | Use2 IdF a
  | UseC c a

data EN3Info c = EN3InfoC
  { _en3Size :: Word,
    _en3Uses :: Maybe (EN3Uses c ()),
    _en3Pair :: IntMap IdF,
    _en3Sym :: Map c IdF
  }

makeLenses ''EN3Info

data Egraph3 enode = Egraph3C
  { _uf3 :: IntMap UF3Info,
    _en3 :: IntMap (EN3Info (Symbol enode))
  }

makeLenses ''Egraph3

efind3 :: IdE -> Lens' (Egraph3 enode) UF3Info
efind3 e =
  uf3
    . at (e ^. unEId3)
    . anon (UF3InfoC (assertCanon e) e) (\(UF3InfoC (EId3C a) (EId3C b)) -> a == b)

eroot3 :: IdE -> Lens' (Egraph3 enode) IdF
eroot3 e = efind3 e . uf3Root

nilInfo :: Iso' (Maybe (EN3Info c)) (EN3Info c)
nilInfo = anon nil isNil
  where
    nil = EN3InfoC 1 Nothing IntMap.empty Map.empty
    isNil (EN3InfoC s u m1 m2) = s == 1 && isNothing u && IntMap.null m1 && Map.null m2

enodeInfo3 :: IdE -> Lens' (Egraph3 enode) (EN3Info (Symbol enode))
enodeInfo3 e f eg = (en3 . at (eg ^. eroot3 e . unEId3) . nilInfo) f eg

eunion3 :: IdE -> IdE -> Egraph3 enode -> (IdF, Egraph3 enode)
eunion3 a b eg = let eg' = executingState eg (epropagate [(a, b)]) in (eg' ^. eroot3 a, eg')

esiblings :: IdE -> Egraph3 enode -> [IdE]
esiblings e eg = go e e
  where
    go a b =
      let UF3InfoC _ nx = eg ^. efind3 b
       in if nx == a then [] else b : go a nx

epropagate :: [(IdE, IdE)] -> State (Egraph3 enode) ()
epropagate [] = pass
epropagate ((a, b) : es) = do
  (newRoot, newChild, newSize) <- do
    a' <- use (eroot3 a)
    b' <- use (eroot3 b)
    sa <- use (enodeInfo3 a . en3Size)
    sb <- use (enodeInfo3 b . en3Size)
    pure $ if sa > sb then (a', b', sa + sb) else (b', a', sa + sb)

  -- reroot the smaller class
  get >>= traverse_ (\sib -> efind3 sib . uf3Root .= newRoot) . esiblings (forgetCanon newChild)
  -- update info
  oldInfo <- en3 . at (newChild ^. unEId3) <<.= Nothing
  en3 . at (newRoot ^. unEId3) . nilInfo . en3Size .= newSize
  -- extend sibling pointers
  efind3 (forgetCanon newChild) . uf3NextSib .= forgetCanon newRoot
  efind3 (forgetCanon newRoot) . uf3NextSib .= forgetCanon newChild
  nxs <- eprop' (forgetCanon newChild) oldInfo
  epropagate ((nxs >>= toList) ++ es)

-- This is too clever by half, as this approach seems to not actually simplify
-- anything...
eprop' :: IdE -> Maybe (EN3Info c) -> State (Egraph3 enode) [Maybe (IdE, IdE)]
eprop' _ Nothing = pure []
eprop' e (Just (EN3InfoC _ s _ _)) = _

-- Attempt N+1
-- for a: E,
--   a → r
--   some b such that b → r (forming a loop...)
-- or just
--   r ← as...
-- for r: F
--   length of as
--   as
--   set of either the a such that (a, r) → c or (r, a) → c
--   map from a to c if (r, a) → c

-- Relations:
-- AtomR(a, r)
-- PairR(a, c, r)
-- App(sym, a, r) with fundep sym, a → r
-- Indexes:
-- AtomR(a, _) since we have fundep a → r
-- AtomR(_, r) with no fundep
-- PairR(a, b, _) since we have fundep (a, c) → r
-- PairR(a, _, x) where we don't need x due to the above fundep
-- PairR(_, c, x) ^^
-- but note for the above: we only need PairR(a, _, x) v PairR(_, a, x)
-- Aa. Atom(a, r1) and Atom(a, r2) => r1 = r2
-- Aab. Pair(a, b, r1) and Pair(a, b, r2) => r1 = r2
