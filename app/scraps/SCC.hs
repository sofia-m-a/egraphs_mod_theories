-- Tarjan 1

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k (Natural, Natural, Bool),
--     _tjstackA :: [k],
--     _tjout :: [[k]]
--   }

-- makeLenses ''TarjanState

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) ()
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= (i, i, True)
--   tjstackA <|= v
--   for_ (graph v) \w ->
--     use (tjvisited . at w) >>= \case
--       Nothing -> do
--         tarjanStep graph w
--         -- Flaw: forgetting that w now has ll
--         wll <- fromMaybe undefined <$> preuse (tjvisited . at w . _Just . _2)
--         tjvisited . at v . _Just . _2 %= min wll
--       Just (n, _, onst) ->
--         when onst $
--           tjvisited . at v . _Just . _2 %= min n
--   -- Flaw
--   (n, ll, _) <- fromMaybe undefined <$> preuse (tjvisited . ix v)
--   when (n == ll) $ buildSCC >>= (tjout <|=)
--   where
--     buildSCC = do
--       -- Flaw
--       (w, rest) <- fromMaybe undefined <$> preuse (tjstackA . _Cons)
--       tjstackA .= rest
--       tjvisited . at w . _Just . _3 .= False
--       if w /= v then (w :) <$> buildSCC else pure [w]

-- Tarjan 2
-- Remove tjout as state; return [[k]]
-- Return ll rather than lookup
-- Work 'list-at-a-time' in buildSCC

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k (Natural, Natural, Bool),
--     _tjstackA :: [k]
--   }

-- makeLenses ''TarjanState

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) (Natural, [[k]])
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= (i, i, True)
--   tjstackA <|= v
--   outs <-
--     concat <$> for (graph v) \w ->
--       use (tjvisited . at w) >>= \case
--         Nothing -> do
--           (wll, ls) <- tarjanStep graph w
--           tjvisited . at v . _Just . _2 %= min wll
--           pure ls
--         Just (n, _, onst) -> do
--           when onst $
--             tjvisited . at v . _Just . _2 %= min n
--           pure []
--   (n, ll, _) <- fromMaybe undefined <$> preuse (tjvisited . ix v)
--   newScc <- if n == ll then (:) <$> buildSCC else pure id
--   pure (ll, newScc outs)
--   where
--     buildSCC = do
--       (w, rest') <- span (/= v) <$> use tjstackA
--       tjstackA .= case rest' of
--         v' : rest'' | v' == v -> rest''
--         _ -> undefined
--       let scc = w ++ [v]
--       for_ scc \ww -> tjvisited . at ww . _Just . _3 .= False
--       pure scc

-- Tarjan 3
-- Analyze the flow of lowlink info
-- lowlink is local!
-- it starts = index, and we update v's lowlink from w's lowlink, which we are returning
-- rather than looking up.
-- Also, we don't need n unless onstack is True
-- So the map is now Map k Progress
-- with
-- data Progress = OnStack Natural | Complete

-- Because lowlink is local, we just need to update it to the minimum value
-- collected from the loop. So, to handle the 'outs' loop, introduce
-- data Outs k = Outs [k] (Maybe Natural)
-- with a Monoid instance that concats [k] and mininums the naturals
-- Also, note that index is never changed, so we don't need to re-look it up

-- data Progress = OnStack Natural | Complete

-- data Outs k = Outs [k] (Maybe Natural)

-- instance Semigroup (Outs k) where
--   Outs is (Just n) <> Outs js (Just m) = Outs (is <> js) (Just (n `min` m))
--   Outs is (Just n) <> Outs js Nothing = Outs (is <> js) (Just n)
--   Outs is Nothing <> Outs js (Just m) = Outs (is <> js) (Just m)
--   Outs is Nothing <> Outs js Nothing = Outs (is <> js) Nothing

-- instance Monoid (Outs k) where
--   mempty = Outs [] Nothing

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k Progress,
--     _tjstackA :: [k]
--   }

-- makeLenses ''TarjanState

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) (Natural, [[k]])
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= OnStack i
--   tjstackA <|= v
--   Outs ks m <-
--     fold <$> for (graph v) \w ->
--       use (tjvisited . at w) >>= \case
--         Nothing -> do
--           (wll, ls) <- tarjanStep graph w
--           pure (Outs ls (Just wll))
--         Just (OnStack n) -> do
--           pure (Outs [] (Just n))
--         Just Complete -> pure (Outs [] Nothing)
--   let (ll, outs') = (maybe i (min i) m, ks)
--   newScc <- if i == ll then (:) <$> buildSCC else pure id
--   pure (ll, newScc outs')
--   where
--     buildSCC = do
--       (w, rest') <- span (/= v) <$> use tjstackA
--       tjstackA .= case rest' of
--         v' : rest'' | v' == v -> rest''
--         _ -> undefined
--       let scc = w ++ [v]
--       for_ scc \ww -> tjvisited . at ww . _Just . _3 .= False
--       pure scc

-- Tarjan 4
-- Rather than have the stack as a mutable variable, focus on the way it changes
-- 1. When we see a node, it gets added to the stack
-- 2a. If `maybe i (min i) m == i`, then it gets popped, together with anything
--       leftover from child calls
-- 2b. If `maybe i (min i) m /= i`, we keep it on the stack for the parent to
--       deal with
-- Indeed, because a child call will never pop the value pushed in the parent,
-- we could even move the line `tjstackA <|= v` until right before buildScc.

-- Instead of the stateful approach, we can pass the 'leftover' part of the stack
-- upwards via the return value.

-- This gives rise to the type `TarjanReturn` which is a Semigroup that extends Outs
-- However, there is one subtlety: we will not use `Maybe Natural` but just use
-- i as a relative unit in order to simplify things for this step.
-- This means TarjanReturn is not a Monoid, so we will handle empty lists
-- explicitly

-- data Progress = OnStack Natural | Complete

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k Progress
--   }

-- makeLenses ''TarjanState

-- data TarjanReturn k = TarjanReturn
--   { _tjlowlink :: Natural,
--     _tjleftover :: [k],
--     _tjsccs :: [[k]]
--   }

-- instance Semigroup (TarjanReturn k) where
--   TarjanReturn ll1 lo1 sc1 <> TarjanReturn ll2 lo2 sc2 = TarjanReturn (ll1 `min` ll2) (lo1 <> lo2) (sc1 <> sc2)

-- makeLenses ''TarjanReturn

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) (TarjanReturn k)
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= OnStack i
--   tr <- case nonEmpty (graph v) of
--     Nothing -> pure (TarjanReturn i [] [])
--     Just ne ->
--       fold1 <$> flip traverse1 ne \w ->
--         use (tjvisited . at w) >>= \case
--           Nothing -> tarjanStep graph w
--           Just (OnStack n) -> do
--             pure (TarjanReturn n [] [])
--           Just Complete -> pure (TarjanReturn i [] [])
--   let tr' = tr & tjlowlink %~ min i
--   if tr' ^. tjlowlink == i
--     then pure (TarjanReturn i [] ((tr' ^. tjleftover |> v) : tr' ^. tjsccs))
--     else pure (tr' & tjleftover |>~ v)

-- Tarjan 5
-- Given that we are now threading the leftovers, we are building it 'in
-- the wrong order'. So, we will build it in reverse and reverse at the end
-- (something for the wrapper to handle!)

-- data Progress = OnStack Natural | Complete

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k Progress
--   }

-- makeLenses ''TarjanState

-- data TarjanReturn k = TarjanReturn
--   { _tjlowlink :: Natural,
--     _tjleftover :: [k],
--     _tjsccs :: [[k]]
--   }

-- instance Semigroup (TarjanReturn k) where
--   TarjanReturn ll1 lo1 sc1 <> TarjanReturn ll2 lo2 sc2 = TarjanReturn (ll1 `min` ll2) (lo1 <> lo2) (sc1 <> sc2)

-- makeLenses ''TarjanReturn

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) (TarjanReturn k)
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= OnStack i
--   tr <- case nonEmpty (graph v) of
--     Nothing -> pure (TarjanReturn i [] [])
--     Just ne ->
--       fold1 <$> flip traverse1 ne \w ->
--         use (tjvisited . at w) >>= \case
--           Nothing -> tarjanStep graph w
--           Just (OnStack n) -> do
--             pure (TarjanReturn n [] [])
--           Just Complete -> pure (TarjanReturn i [] [])
--   let tr' = tr & tjlowlink %~ min i & tjleftover <|~ v
--   if tr' ^. tjlowlink == i
--     then pure (TarjanReturn i [] ((tr' ^. tjleftover) : tr' ^. tjsccs))
--     else pure tr'

-- Tarjan 6
-- We will extract some key steps

-- data Progress = OnStack Natural | Complete

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k Progress
--   }

-- makeLenses ''TarjanState

-- data TarjanReturn k = TarjanReturn
--   { _tjlowlink :: Natural,
--     _tjleftover :: [k],
--     _tjsccs :: [[k]]
--   }

-- instance Semigroup (TarjanReturn k) where
--   TarjanReturn ll1 lo1 sc1 <> TarjanReturn ll2 lo2 sc2 = TarjanReturn (ll1 `min` ll2) (lo1 <> lo2) (sc1 <> sc2)

-- makeLenses ''TarjanReturn

-- rootSCC :: Natural -> k -> TarjanReturn k -> TarjanReturn k
-- rootSCC i v tr =
--   let tr' = tr & tjlowlink %~ min i & tjleftover <|~ v
--    in if tr' ^. tjlowlink == i
--         then TarjanReturn i [] ((tr' ^. tjleftover) : tr' ^. tjsccs)
--         else tr'

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) (TarjanReturn k)
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= OnStack i
--   tr <- case nonEmpty (graph v) of
--     Nothing -> pure (TarjanReturn i [] [])
--     Just ne ->
--       fold1 <$> flip traverse1 ne \w ->
--         use (tjvisited . at w) >>= \case
--           Nothing -> tarjanStep graph w
--           Just (OnStack n) -> do
--             pure (TarjanReturn n [] [])
--           Just Complete -> pure (TarjanReturn i [] [])
--   pure (rootSCC i v tr)

-- Tarjan 2.1

-- data Progress = OnStack Natural | Complete

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k Progress
--   }

-- makeLenses ''TarjanState

-- type SC k = Tree (k, [(Natural, )])

-- type SCC k = (Natural, SCCTree k)

-- data SCCTree k = SCCNode k [SCCTree k] [(Natural, SCCTree k)]
--   deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- topSCC :: SCCTree k -> Tree k
-- topSCC (SCCNode k ts _) = Node k (fmap topSCC ts)

-- nextSCCs :: SCCTree k -> [(Natural, SCCTree k)]
-- nextSCCs (SCCNode _ ns sccs) = sccs ++ concatMap nextSCCs ns

-- flattenSCCx :: SCCTree k -> [[k]]
-- flattenSCCx n@(SCCNode k ts cs) = toList (topSCC n) : concatMap (flattenSCCx . snd) (nextSCCs n)

-- sccBob :: (Ord k) => (k -> [k]) -> [k] -> [[k]]
-- sccBob gr v = concatMap (flattenSCCx . snd) $ tarjanSCC' gr v

-- tarjanSCC :: (Ord k) => (k -> [k]) -> k -> SCC k
-- tarjanSCC graph v = evaluatingState (TarjanStateC 0 mempty) (tarjanStep graph v)

-- tarjanSCC' :: (Ord k) => (k -> [k]) -> [k] -> [SCC k]
-- tarjanSCC' graph vs =
--   concat $
--     evaluatingState
--       (TarjanStateC 0 mempty)
--       ( for vs $ \v ->
--           use (tjvisited . at v) >>= \case
--             Nothing -> tarjanStep graph v <&> pure
--             Just _ -> pure []
--       )

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) (SCC k)
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= OnStack i
--   (ns, ts, sccs) <-
--     unzip3 <$> for (graph v) \w ->
--       use (tjvisited . at w) >>= \case
--         Nothing -> do
--           (j, n) <- tarjanStep graph w
--           if j <= i
--             then pure (j, [n], [])
--             else pure (j, [], [(j, n)])
--         Just (OnStack j) -> pure (j, [], [])
--         Just Complete -> pure (i, [], [])
--   let d = minimum (i :| ns)
--   let out = SCCNode v (concat ts) (concat sccs)
--   when (d == i) $
--     for_ (topSCC out) (\u -> tjvisited . at u ?= Complete)
--   pure (d, out)

-- Tarjan 2.2
-- newtype SCCTree k = SCCTree (Tree (k, [RootedSCCTree k]))
--   deriving (Show)

-- type RootedSCCTree k = (Natural, SCCTree k)

-- sccNode :: k -> [RootedSCCTree k] -> [SCCTree k] -> SCCTree k
-- sccNode v sccs cs = SCCTree (Node (v, sccs) (fmap (\(SCCTree u) -> u) cs))

-- unsccNodes :: SCCTree k -> [k]
-- unsccNodes (SCCTree t) = toList (fmap fst t)

-- data Progress = OnStack Natural | Complete

-- data TarjanState k = TarjanStateC
--   { _tjcounter :: Natural,
--     _tjvisited :: Map k Progress
--   }

-- makeLenses ''TarjanState

-- data ProcessedChild k
--   = Subtree (RootedSCCTree k)
--   | Backpointer Natural
--   | Component

-- processChild :: ProcessedChild k -> (Natural, [SCCTree k], [RootedSCCTree k]) -> (Natural, [SCCTree k], [RootedSCCTree k])
-- processChild (Subtree (low, tr)) (i, trs, sccs)
--   | low <= i = (low, tr : trs, sccs)
--   | otherwise = (i, trs, (low, tr) : sccs)
-- processChild (Backpointer low) (i, trs, sccs) = (i `min` low, trs, sccs)
-- processChild Component s = s

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (TarjanState k) (RootedSCCTree k)
-- tarjanStep graph v = do
--   i <- tjcounter <<+= 1
--   tjvisited . at v ?= OnStack i
--   nx <- for (graph v) \w ->
--     use (tjvisited . at w) >>= \case
--       Nothing -> Subtree <$> tarjanStep graph w
--       Just (OnStack j) -> pure (Backpointer j)
--       Just Complete -> pure Component
--   let (d, ts, sccs) = flipfoldl' processChild (i, [], []) nx
--   let out = sccNode v sccs ts
--   when (d == i) $
--     for_ (unsccNodes out) (\u -> tjvisited . at u ?= Complete)
--   pure (d, out)

-- tarjanSCC :: (Ord k) => (k -> [k]) -> k -> RootedSCCTree k
-- tarjanSCC graph v = evaluatingState (TarjanStateC 0 mempty) (tarjanStep graph v)

-- tarjanSCC' :: (Ord k) => (k -> [k]) -> [k] -> [RootedSCCTree k]
-- tarjanSCC' graph vs =
--   concat $
--     evaluatingState
--       (TarjanStateC 0 mempty)
--       ( for vs $ \v ->
--           use (tjvisited . at v) >>= \case
--             Nothing -> tarjanStep graph v <&> pure
--             Just _ -> pure []
--       )

-- Tarjan 2.3
-- No lensoid stuff, explicit data structures
-- data SCCTree k = SCCNode k [SCCChild k]

-- data SCCChild k
--   = SubtreeOfThisSCC (SCCTree k)
--   | ChildSCC (SCCTree k)
--   | Backpointer
--   | Component

-- listSCCPre :: SCCTree k -> [k]
-- listSCCPre = head . fmap toList . listSCCsPreNE

-- listSCCsPre :: SCCTree k -> [[k]]
-- listSCCsPre = toList . fmap toList . listSCCsPreNE

-- listSCCsPreNE :: SCCTree k -> NonEmpty (NonEmpty k)
-- listSCCsPreNE n = uncurry (:|) (go n ([], []))
--   where
--     go :: SCCTree k -> ([k], [NonEmpty k]) -> (NonEmpty k, [NonEmpty k])
--     go (SCCNode k cs) s = first (k :|) $ foldr goC s cs

--     goC :: SCCChild k -> ([k], [NonEmpty k]) -> ([k], [NonEmpty k])
--     goC (SubtreeOfThisSCC tr) = first toList . go tr
--     goC (ChildSCC tr) = ([],) . uncurry (:) . go tr
--     goC Backpointer = id
--     goC Component = id

-- data Progress = OnStack Natural | Complete

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (Natural, Map k Progress) (Natural, SCCTree k)
-- tarjanStep graph v = do
--   i <- state (\(i, m) -> (i, (i + 1, Map.insert v (OnStack i) m)))
--   (ds, ts) <-
--     unzip <$> for (graph v) \w ->
--       gets (Map.lookup w . snd) >>= \case
--         Nothing ->
--           tarjanStep graph w <&> \(j, tr) ->
--             if j <= i
--               then (Just j, SubtreeOfThisSCC tr)
--               else (Nothing, ChildSCC tr)
--         Just (OnStack j) -> pure (Just j, Backpointer)
--         Just Complete -> pure (Nothing, Component)
--   let d = foldr (maybe id min) i ds
--   let out = SCCNode v ts
--   when (d == i) $
--     for_ (listSCCPre out) (\w -> modify (second (Map.insert w Complete)))
--   pure (d, out)

-- tarjanSCC :: (Ord k) => (k -> [k]) -> k -> [[k]]
-- tarjanSCC graph v = listSCCsPre . snd $ evalState (tarjanStep graph v) (0, mempty)

-- tarjanSCC' :: (Ord k) => (k -> [k]) -> [k] -> [[k]]
-- tarjanSCC' graph vs =
--   concat $
--     evalState
--       ( for vs $ \v ->
--           gets (Map.lookup v . snd) >>= \case
--             Nothing -> tarjanStep graph v <&> listSCCsPre . snd
--             Just _ -> pure []
--       )
--       (0, mempty)

-- Tarjan 2.4: explicit difference lists (awaiting defunctionalization)
-- type DList a = Endo [a]

-- emptyD :: DList a
-- emptyD = Endo id

-- consD :: a -> DList a -> DList a
-- consD a = Endo . ((a :) .) . appEndo

-- toListD :: DList a -> [a]
-- toListD = flip appEndo []

-- data SCCNode k = SCCNode (Min Natural) (DList k) (DList [k])
--   deriving (Generic)
--   deriving (Semigroup) via (Generically (SCCNode k))

-- data Progress = OnStack Natural | Complete

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (Natural, Map k Progress) (SCCNode k)
-- tarjanStep graph v = do
--   i <- state (\(i, m) -> (i, (i + 1, Map.insert v (OnStack i) m)))
--   ns <- for (graph v) \w ->
--     gets (Map.lookup w . snd) >>= \case
--       Nothing ->
--         tarjanStep graph w <&> \(SCCNode (Min j) ks kss) ->
--           if j <= i
--             then SCCNode (Min j) ks kss
--             else SCCNode (Min i) ks kss
--       Just (OnStack j) -> pure (SCCNode (Min j) emptyD emptyD)
--       Just Complete -> pure (SCCNode (Min i) emptyD emptyD)
--   let n@(SCCNode (Min d) ks kss) = foldr (<>) (SCCNode (Min i) emptyD emptyD) ns
--   if d == i
--     then do
--       let ks' = v : toListD ks
--       for_ ks' (\w -> modify (second (Map.insert w Complete)))
--       pure $ SCCNode (Min d) emptyD (consD ks' kss)
--     else pure n

-- tarjanSCC :: (Ord k) => (k -> [k]) -> k -> [[k]]
-- tarjanSCC graph v = (\(SCCNode _ _ kss) -> toListD kss) $ evalState (tarjanStep graph v) (0, mempty)

-- tarjanSCC' :: (Ord k) => (k -> [k]) -> [k] -> [[k]]
-- tarjanSCC' graph vs =
--   toListD $
--     fold $
--       evalState
--         ( for vs $ \v ->
--             gets (Map.lookup v . snd) >>= \case
--               Nothing -> tarjanStep graph v <&> (\(SCCNode _ _ kss) -> kss)
--               Just _ -> pure emptyD
--         )
--         (0, mempty)

-- Tarjan 2.5: defunctionalize
-- data DList a
--   = EmptyD
--   | ConsD a (DList a)
--   | ConcatD (DList a) (DList a)
--   deriving (Functor, Foldable, Traversable)

-- toListD :: DList a -> [a]
-- toListD l = appDList l []

-- appDList :: DList a -> [a] -> [a]
-- appDList EmptyD = id
-- appDList (ConsD a as) = (a :) . appDList as
-- appDList (ConcatD as bs) = appDList as . appDList bs

-- instance Monoid (DList a) where
--   mempty = EmptyD

-- instance Semigroup (DList a) where
--   (<>) = ConcatD

-- data SCCNode k = SCCNode (Min Natural) (DList k) (DList [k])
--   deriving (Generic)
--   deriving (Semigroup) via (Generically (SCCNode k))

-- data Progress = OnStack Natural | Complete

-- tarjanStep :: (Ord k) => (k -> [k]) -> k -> State (Natural, Map k Progress) (SCCNode k)
-- tarjanStep graph v = do
--   i <- state (\(i, m) -> (i, (i + 1, Map.insert v (OnStack i) m)))
--   ns <- for (graph v) \w ->
--     gets (Map.lookup w . snd) >>= \case
--       Nothing -> tarjanStep graph w
--       Just (OnStack j) -> pure (SCCNode (Min j) EmptyD EmptyD)
--       Just Complete -> pure (SCCNode (Min i) EmptyD EmptyD)
--   let n@(SCCNode (Min d) ks kss) = foldl' (<>) (SCCNode (Min i) (ConsD v EmptyD) EmptyD) ns
--   if d == i
--     then do
--       let ks' = toListD ks
--       for_ ks' (\w -> modify (second (Map.insert w Complete)))
--       pure $ SCCNode (Min d) EmptyD (ConsD ks' kss)
--     else pure n

-- tarjanSCC :: (Ord k) => (k -> [k]) -> k -> [[k]]
-- tarjanSCC graph v = toListD $ (\(SCCNode _ _ kss) -> kss) $ evalState (tarjanStep graph v) (0, mempty)

-- tarjanSCC' :: (Ord k) => (k -> [k]) -> [k] -> [[k]]
-- tarjanSCC' graph vs =
--   toListD $
--     fold $
--       evalState
--         ( for vs $ \v ->
--             gets (Map.lookup v . snd) >>= \case
--               Nothing -> tarjanStep graph v <&> (\(SCCNode _ _ kss) -> kss)
--               Just _ -> pure EmptyD
--         )
--         (0, mempty)

-- ggg :: Int -> [Int]
-- ggg 0 = [1, 2, 3]
-- ggg 1 = [0, 2]
-- ggg 2 = [3]
-- ggg 3 = [3, 6]
-- ggg 4 = [3, 4]
-- ggg 5 = [5, 5, 5]
-- ggg 6 = [8, 4, 6, 7]
-- ggg 8 = [4]
-- ggg _ = []

-- data Treee a = Reoot a [Treee a]

-- flatten :: Treee a → [a]
-- flatten t = go t []
--   where
--     go (Reoot a cs) acc = a : foldr go acc cs
