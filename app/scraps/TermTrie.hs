{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TermTrie (TermTrie, emptyTT) where

import Control.Lens
import Data.Map.Merge.Strict (dropMissing, merge, preserveMissing, zipWithMatched)
import Data.Map.Strict qualified as Map
import Lude

-- Simple tries
data TermTrie a r = TermTrieC
  { _smRoot :: Maybe r,
    _smLeaves :: Map a (TermTrie a r)
  }
  deriving (Eq, Ord, Functor, Show)

makeLenses ''TermTrie

instance Foldable (TermTrie r) where
  foldMap f (TermTrieC r ls) = foldMap f r <> foldMap (foldMap f) ls

instance Traversable (TermTrie r) where
  traverse f (TermTrieC r ls) = TermTrieC <$> traverse f r <*> traverse (traverse f) ls

instance One (TermTrie a r) where
  type OneItem (TermTrie a r) = ([a], r)
  one :: ([a], r) -> TermTrie a r
  one (as, r) =
    foldr
      (\a tr -> TermTrieC Nothing (one (a, tr)))
      (TermTrieC (Just r) Map.empty)
      as

emptyTT :: TermTrie a r
emptyTT = TermTrieC Nothing Map.empty

thesel :: (Semialign f) => (These a b -> c) -> These (f a) (f b) -> f c
thesel c1 (These fa fb) = alignWith c1 fa fb
thesel c1 (This fa) = fmap (c1 . This) fa
thesel c1 (That fb) = fmap (c1 . That) fb

instance (Ord k) => Semialign (TermTrie k) where
  alignWith ::
    (Ord k) =>
    (These a b -> c) ->
    TermTrie k a ->
    TermTrie k b ->
    TermTrie k c
  alignWith combine = go
    where
      go (TermTrieC r1 ls1) (TermTrieC r2 ls2) =
        TermTrieC
          (alignWith combine r1 r2)
          (alignWith (thesel combine) ls1 ls2)

-- union = align
-- intersect = zipWith
instance (Ord k) => Align (TermTrie k) where
  nil = TermTrieC Nothing Map.empty

instance (Ord k) => Zip (TermTrie k) where
  zipWith combine = go
    where
      go (TermTrieC r1 ls1) (TermTrieC r2 ls2) =
        TermTrieC (zipWith combine r1 r2) (zipWith go ls1 ls2)

instance (Ord k) => Unzip (TermTrie k) where
  unzip = unzipDefault

-- if (as, f) is in t1, and (bs, x) is in t2, then (as ++ bs, f x) is in t1 <*> t2
instance (Ord k) => Applicative (TermTrie k) where
  pure a = TermTrieC (pure a) Map.empty

  -- Product rule, if you squint...
  TermTrieC r1 ls1 <*> t2@(TermTrieC r2 ls2) =
    TermTrieC
      (r1 <*> r2)
      (fmap (<*> t2) ls1 <> foldMap (<<$>> ls2) r1)

type instance Index (TermTrie k a) = [k]

type instance IxValue (TermTrie k a) = a

instance (Ord k) => Ixed (TermTrie k a)

isEmptyTT :: TermTrie k a -> Bool
isEmptyTT (TermTrieC Nothing m) | Map.null m = True
isEmptyTT _ = False

instance (Ord k) => At (TermTrie k a) where
  -- the 'anon' lens is used to delete empty parts of the Trie.
  -- What would be Just (TermTrieC Nothing Map.empty) gets mapped to
  -- Nothing
  at ::
    (Ord k) =>
    [k] -> Lens' (TermTrie k a) (Maybe (IxValue (TermTrie k a)))
  at [] = smRoot
  at (k : ks) = smLeaves . at k . anon emptyTT isEmptyTT . at ks

-- Older experiments
-- In particular, TermTries where we don't just want to store and access t a
-- but to index on t a while being able to access t b, as if we freeze a traversal.
--
-- Bazaar
-- ======
-- A non-regular data type that represents a Traversal applied to an argument
-- NB: Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)
-- Convert a Traversal to a Bazaar
--   toBZ :: Traversal s t a b -> s -> BZ a b (t b)
-- Convert a Bazaar to a Traversal
--   froBZ :: (Applicative f) => BZ a b t -> (a -> f b) -> f t
--
-- Equivalent to exists n. (a^n, b^n -> t)
-- profunctor in a, b
-- functor in t
-- Applicative:
--   pure t is (a^0, b^0 -> t)
--   (a^n, b^n -> (s -> t)) <*> (a^m, b^m -> s) ==> (a^(m+n), b^(m+n) -> t)
-- pseudo-Comonad:
--   extract: (a^n, a^n -> t) -> t
--   cojoin :: (a^n, b^n -> t) ---> (a^n, c^n -> c^n, b^n -> t)

data BZ a b t
  = Done t
  | More a (BZ a b (b -> t))
  deriving (Functor)

instance Applicative (BZ a b) where
  pure = Done

  (<*>) :: BZ a b (s -> t) -> BZ a b s -> BZ a b t
  Done tf <*> c = fmap tf c
  More a z <*> c = More a (flip <$> z <*> c)

unBZ :: BZ a a t -> t
unBZ (Done t) = t
unBZ (More a k) = unBZ k a

coBZ :: BZ a b t -> BZ a c (BZ c b t)
coBZ (Done t) = Done (pure t)
coBZ (More a k) = More a (fmap (flip More) (coBZ k))

sell :: a -> BZ a b b
sell a = More a (Done id)

toBZ :: Traversal s t a b -> s -> BZ a b t
toBZ t = t sell

froBZ :: (Applicative f) => BZ a b t -> (a -> f b) -> f t
froBZ (Done t) _ = pure t
froBZ (More a k) r = froBZ k r <*> r a

bz :: (Traversable t) => t a -> BZ a b (t b)
bz = traverse sell

-- A shared Bazaar

data TermTrieR a b t
  = TermTrieRC
  { _ttMap :: Map a (TermTrieR a b (b -> t)),
    _ttRes :: Maybe t
  }
  deriving (Functor)

makeLenses ''TermTrieR

toListTT :: TermTrieR a a t -> [t]
toListTT (TermTrieRC m r) = toList r ++ ifoldMap (\k tt -> fmap ($ k) (toListTT tt)) m

coTT :: TermTrieR a b t -> TermTrieR a c (TermTrieR c b t)
coTT (TermTrieRC m r) = TermTrieRC (fmap (fmap (\f k -> TermTrieRC (one (k, f)) Nothing) . coTT) m) (fmap pureTT r)

pureTT :: t -> TermTrieR a b t
pureTT t = TermTrieRC Map.empty (pure t)

intersectTT :: (Ord a) => TermTrieR a b (s -> t) -> TermTrieR a b s -> TermTrieR a b t
intersectTT (TermTrieRC m1 r1) (TermTrieRC m2 r2) =
  TermTrieRC
    (merge dropMissing dropMissing (zipWithMatched (\_ a b -> ((<*>) <$> a) `intersectTT` b)) m1 m2)
    (r1 <*> r2)

productTT :: (Ord a) => TermTrieR a b (s -> t) -> TermTrieR a b s -> TermTrieR a b t
productTT (TermTrieRC m1 r1) t2@(TermTrieRC m2 r2) =
  TermTrieRC (fmap (\tt -> (flip <$> tt) `productTT` t2) m1 <> co) (r1 <*> r2)
  where
    co = case r1 of
      Just f -> (f .) <<$>> m2
      Nothing -> mempty

instance (Ord a) => Applicative (TermTrieR a b) where
  pure = pureTT
  (<*>) = productTT

unionTT :: (Ord a, Semigroup t) => TermTrieR a b t -> TermTrieR a b t -> TermTrieR a b t
unionTT (TermTrieRC m1 r1) (TermTrieRC m2 r2) =
  TermTrieRC (merge preserveMissing preserveMissing (zipWithMatched (const unionTT)) m1 m2) (r1 <> r2)

instance (Ord a, Semigroup t) => Semigroup (TermTrieR a b t) where
  (<>) = unionTT

instance (Ord a, Semigroup t) => Monoid (TermTrieR a b t) where
  mempty = TermTrieRC mempty Nothing

fromBZ :: BZ a b t -> TermTrieR a b t
fromBZ (Done t) = TermTrieRC Map.empty (Just t)
fromBZ (More a k) = TermTrieRC (one (a, fromBZ k)) Nothing

sellTT :: a -> TermTrieR a b b
sellTT a = TermTrieRC (one (a, TermTrieRC Map.empty (Just id))) Nothing

oneTT :: (Traversable f) => f a -> TermTrieR a a (f a)
oneTT = fromBZ . bz

emptyTTR :: TermTrieR a b t
emptyTTR = TermTrieRC Map.empty Nothing

toSimpleTrie :: TermTrieR a a t -> TermTrie a t
toSimpleTrie (TermTrieRC m r) = TermTrieC r (imap (\a -> toSimpleTrie . fmap ($ a)) m)