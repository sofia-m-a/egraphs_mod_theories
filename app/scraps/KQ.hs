-- data KQueue a
--   = KLeaf a
--   | KBranch (KQueue a) (KQueue a)

-- singleK :: a -> KQueue a
-- singleK = KLeaf

-- enqueueK :: a -> KQueue a -> KQueue a
-- enqueueK = KBranch . KLeaf

-- dequeueK1 :: KQueue a -> (Maybe (KQueue a), a)
-- dequeueK1 (KLeaf a) = (Nothing, a)
-- dequeueK1 (KBranch q r) = case r of
--   KLeaf a -> (Just q, a)
--   KBranch s t -> dequeueK1 (KBranch (KBranch q s) t)

-- dequeueK2 :: KQueue a -> (Maybe (KQueue a), a)
-- dequeueK2 (KLeaf a) = (Nothing, a)
-- dequeueK2 (KBranch q r) = case r of
--   KLeaf a -> (Just q, a)
--   KBranch s t -> case dequeueK2 t of
--     (Nothing, a) -> (Just (KBranch q s), a)
--     (Just p, a) -> (Just (KBranch (KBranch q s) p), a)

-- dequeueK3 :: KQueue a -> (Maybe (KQueue a), a)
-- dequeueK3 (KLeaf a) = (Nothing, a)
-- dequeueK3 (KBranch q r) = case r of
--   KLeaf a -> (Just q, a)
--   KBranch s t -> first (\m -> Just $ maybe id (flip KBranch) m (KBranch q s)) (dequeueK3 t)

-- data K4Queue a
--   = K4Leaf a
--   | K4Branch (K4Queue a) a
--   | K4Push a (K4Queue a)

-- singleK4 :: a -> K4Queue a
-- singleK4 = K4Leaf

-- enqueueK4 :: a -> K4Queue a -> K4Queue a
-- enqueueK4 = K4Push

-- dequeueK4 :: K4Queue a -> (Maybe (K4Queue a), a)
-- dequeueK4 (K4Leaf a) = (Nothing, a)
-- dequeueK4 (K4Branch k4q a) = (Just k4q, a)
-- dequeueK4 (K4Push a k4a) = goPush (K4Leaf a) k4a
--   where
--     goPush :: K4Queue a -> K4Queue a -> (Maybe (K4Queue a), a)
--     goPush q (K4Leaf b) = (Just q, b)
--     goPush q (K4Branch k4a r) = (Just _, r)
--     goPush q (K4Push b k4a) = goPush (K4Branch q b) k4a

-- data KUnQ a
--   = KUnPush a (KUnQ a)
--   | KUnDone (KQ a)

-- data KQ a
--   = KQ [KQ a] a

-- singleKQ :: a -> KUnQ a
-- singleKQ a = KUnDone (KQ [] a)

-- enqueueKQ :: a -> KUnQ a -> KUnQ a
-- enqueueKQ = KUnPush

-- dequeueKQ :: KUnQ a -> KQ a
-- dequeueKQ (KUnDone kq) = kq
-- dequeueKQ (KUnPush a kuq) = _

-- dequeueKQ' :: KQ a -> (Maybe (KQ a), a)
-- dequeueKQ' (KQ ls a) = (case ls of
--     KQ rs b : ls' -> Just (KQ (_ : ls') b)
--     [] -> Nothing
--   , a)

{-
  data Q1 a = Br1 (Q1 a) a | Br1' [a] a
  data Q2 a = Br2 a (Q2 a)

  deq1 : Q1 a -> Q2 a
  deq1 ((a*b)*c) = deq2 (a*(b*c))
-}

-- data KR a = KEmpty | KNode a [KR a]

-- data KQ a
--   = Enqueue (KQ a) a | KQ2 a (KR a)

-- deq :: KQ a -> (Maybe (KQ a), a)
-- deq (Enqueue kq a) = case kq of
--   Enqueue kq' b -> _
--   KQ2 b bs -> _
-- deq (KQ2 a KEmpty) = (Nothing, a)
-- deq (KQ2 a (KNode krs)) = (Just (KQ2 b bs), a)

-- data KQ a = KQB (KQ a) (KQ a) | KL a | KQR (NonEmpty (KQ a))

-- deq :: KQ a -> (Maybe (KQ a), a)
-- deq (KL a) = (Nothing, a)
-- deq (KQB r s) = first (Just . KQR) $ deq' r (one s)
-- deq (KQR (r :| [])) = deq r
-- deq (KQR (r :| (r2 : rs))) = first (Just . KQR) $ deq' r (r2 :| rs)

-- deq' :: KQ a -> NonEmpty (KQ a) -> (NonEmpty (KQ a), a)
-- deq' (KL a) q = (q, a)
-- deq' (KQB r s) q = deq' r (one s <> q)
-- deq' (KQR (r :| [])) q = deq' r q
-- deq' (KQR (r :| (r2 : rs))) q = deq' r (one (KQR (r2 :| rs)) <> q)

{-

one a = (Leaf a $2)
enq a q = (q * (Leaf a $2) $2)

deq (Leaf a $_) = (Nothing, a)
deq (r * s $2) = first Just $ deq' r s $1
deq (r2 , s $1) = deq' r s $1

deq' (r1 * r2 $2) s $1 = deq' r1 (r2, s $1)
deq' (Leaf a $_) s $_ = (Just s, a)
deq' (r1 , r2 $1) s $1 = deq' r1 (r2, s $1)
-}

-- data KQueue a = KLeaf a | KQueue a :<<: KQueue a | KQueue a :>>: KQueue a

-- singleQ :: a -> KQueue a
-- singleQ = KLeaf

-- enqueueQ :: a -> KQueue a -> KQueue a
-- enqueueQ a q = KLeaf a :<<: q

-- dequeueQ :: KQueue a -> (Maybe (KQueue a), a)
-- dequeueQ (KLeaf a) = (Nothing, a)
-- dequeueQ (r :<<: s) = first Just $ dequeueL r s
-- dequeueQ (r :>>: s) = first Just $ dequeueL r s

-- dequeueL :: KQueue a -> KQueue a -> (KQueue a, a)
-- dequeueL q (KLeaf a) = (q, a)
-- dequeueL q (r1 :<<: r2) = dequeueL (q :>>: r1) r2
-- dequeueL q (r1 :>>: r2) = _

{-
  deq (Leaf a $1) = (Nothing, a)
  deq (Branch r s $1) = first Just $ deq' r s $1

  deq' (Leaf a $1) s $1 = (s, a)
  deq' (Branch r1 r2 $1) s $1 = deq' r1 (Branch r2 s $1) $1

pot q = pot' q 1
pot (L a) n = n
pot (r * s) n = 1 + pot r (n+1) + pot s n

pot (L a * s) = 2

pot (deq (r * s)) = pot (deq' r s)

pot (deq' (Leaf a) s) = pot s
pot (deq' (r1 * r2) s) = pot (deq' r1 (r2 * s)) - 1

1 + pot r 0 + pot s 1

1 + pot r1 (n+2) + pot r2 (n+1) + pot s n
=>
1 + pot r1 (n+1) + pot r2 (n+1) + pot s n

-}