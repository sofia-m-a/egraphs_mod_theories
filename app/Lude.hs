{-# LANGUAGE NoImplicitPrelude #-}

module Lude
  ( module Relude,
    module Control.Applicative.Combinators,
    module Data.Semialign,
    module Data.These,
    module Data.Traversable,
    module Witherable,
    extendingState,
    extendingState',
    focusingState,
  )
where

import Control.Lens (Lens', (^.), (.=))
import Control.Applicative.Combinators (choice)
import Data.Semialign (Align (..), Repeat (..), Semialign (..), Unzip (..), Zip (..), unzipDefault)
import Data.These (These (..), mergeThese, these)
import Data.Traversable (for)
import Relude hiding (catMaybes, filter, mapMaybe, repeat, uncons, unzip, zip, zipWith)
import Witherable (Filterable (..), Witherable (..))

extendingState :: a -> State (s, a) b -> State s (b, a)
extendingState a u = do
  s <- get
  let (b, (s', a')) = runState u (s, a)
  put s'
  pure (b, a')

extendingState' :: a -> State (s, a) () -> State s a
extendingState' a u = fmap snd (extendingState a u)

-- Somewhat unprincipled...
focusingState :: Lens' s a -> State (s, a) b -> State s b
focusingState l m = do
  s <- get
  let (b, (s', a)) = runState m (s, s ^. l)
  id .= s'
  l .= a
  pure b

-- Note: very old custom prelude, here for future reference for myself as to things I
-- 'commonly' use

-- module Lude (
--   module Control.Comonad,
--     module Control.Comonad.Env,
--     module Control.Comonad.Hoist.Class,
--     module Control.Comonad.Identity,
--     module Control.Comonad.Store,
--     module Control.Comonad.Store.Pointer,
--     module Control.Comonad.Store.Zipper,
--     module Control.Comonad.Traced,
--     module Control.Comonad.Trans.Env,
--     module Control.Comonad.Trans.Store,
--     module Control.Comonad.Trans.Traced,
--     module Control.Lens,
--     module Control.Monad.Trans.Can,
--     module Control.Monad.Trans.Smash,
--     module Control.Monad.Trans.Wedge,
--     module Data.Align,
--     module Data.Array.IArray,
--     module Data.Array.IO,
--     module Data.Array.MArray.Safe,
--     module Data.Array.ST,
--     module Data.Array.Storable,
--     module Data.Array.Unboxed,
--     module Data.Bifunctor.Assoc,
--     module Data.Bifunctor.Biff,
--     module Data.Bifunctor.Clown,
--     module Data.Bifunctor.Flip,
--     module Data.Bifunctor.Join,
--     module Data.Bifunctor.Joker,
--     --module Data.Bifunctor.Product,
--     --module Data.Bifunctor.Sum,
--     module Data.Bifunctor.Swap,
--     module Data.Bifunctor.Tannen,
--     module Data.Can,
--     module Data.Crosswalk,
--     module Data.Fix,
--     module Data.Functor.Base,
--     -- module Data.Functor.Combinator,
--     module Data.Functor.Foldable,
--     module Data.Functor.These,
--     module Data.Ix,
--     module Data.Profunctor,
--     module Data.Profunctor.Choice,
--     module Data.Profunctor.Closed,
--     module Data.Profunctor.Mapping,
--     module Data.Profunctor.Ran,
--     module Data.Profunctor.Rep,
--     module Data.Profunctor.Sieve,
--     module Data.Profunctor.Strong,
--     -- module Data.Profunctor.Traversing,
--     module Data.Profunctor.Yoneda,
--     module Data.Semialign,
--     module Data.Smash,
--     module Data.Tagged,
--     module Data.These,
--     module Data.These.Combinators,
--     module Data.Time.Calendar,
--     module Data.Time.Clock,
--     module Data.Time.LocalTime,
--     module Data.Traversable,
--     module Data.Validation,
--     module Data.Wedge,
--     module Data.Zip,
--     module Relude.Applicative,
--     module Relude.Base,
--     module Relude.Bool,
--     module Relude.Container,
--     module Relude.Debug,
--     module Relude.DeepSeq,
--     module Relude.Enum,
--     module Relude.Exception,
--     module Relude.File,
--     module Relude.Foldable,
--     module Relude.Function,
--     module Relude.Functor,
--     module Relude.Lifted,
--     module Relude.List,
--     module Relude.Monad,
--     module Relude.Monoid,
--     module Relude.Numeric,
--     module Relude.Print,
--     module Relude.String,
--     map,
--     (++),
--     -- mapIArray,
--     -- mapIIndices,
--     -- getIndices,
--     askW,
--     asksW,
--     localW,
--     traceW,
--     -- zipperSize,
--     -- Filterable (..),
--     -- partitionEithers,
--     -- partitionWith,
--     -- filter,
--     -- lefts,
--     -- rights,
--     -- (<$?>),
--     -- (<&?>),
--     -- partitionThese,
--     -- partitionHereThere,
--     -- catThis,
--     -- gatherCans,
--     -- gatherWedges,
--     -- gatherSmashes,
--     -- ones,
--     -- enos,
--     -- twos,
--     -- heres,
--     -- theres,
--     -- smashes,
--     -- filterOnes,
--     -- filterTwos,
--     -- filterEnos,
--     -- filterNons,
--     -- filterNadas,
--     -- filterHeres,
--     -- filterTheres,
--     -- filterNowheres,
--     -- partitionSplitCans,
--     -- partitionEitherToCan,
--     -- catThat,
--     -- catThese,
--     -- catHere,
--     -- catThere,
--     -- Witherable (..),
--     -- witherEither,
--     -- filterA,
--     -- witherMap,
--     -- eitherToValidation,
--     -- validationToEither,
--     -- validateOrElse,
--     -- Unfolding (..),
--     -- Biproduct,
--     -- Bisum,
--   )
-- where

-- import Control.Comonad
-- import Control.Comonad.Env hiding (ask, asks, local)
-- import Control.Comonad.Env as Env
-- import Control.Comonad.Hoist.Class
-- import Control.Comonad.Identity
-- import Control.Comonad.Store
-- import Control.Comonad.Store.Pointer
-- import Control.Comonad.Store.Zipper hiding (size)
-- import Control.Comonad.Store.Zipper as StoreZipper
-- import Control.Comonad.Traced hiding (trace)
-- import Control.Comonad.Traced as Trace
-- import Control.Comonad.Trans.Env (Env, EnvT (..), env, lowerEnvT, runEnv, runEnvT)
-- import Control.Comonad.Trans.Store (Store, StoreT (..), runStore, runStoreT, store)
-- import Control.Comonad.Trans.Traced hiding (trace)
-- import Control.Lens hiding (index, indices, para, universe)
-- import Control.Lens qualified
-- import Control.Monad.Trans.Can
-- import Control.Monad.Trans.Smash
-- import Control.Monad.Trans.Wedge
-- import Data.Align
-- import Data.Array.IArray (Array, IArray, accum, accumArray, array, assocs, bounds, elems, indices, listArray, (!), (//))
-- import Data.Array.IArray qualified as IArray
-- import Data.Array.IO (IOArray, IOUArray, hGetArray, hPutArray)
-- import Data.Array.MArray.Safe (MArray, freeze, getAssocs, getBounds, getElems, mapArray, mapIndices, newArray, newListArray, readArray, thaw, writeArray)
-- import Data.Array.ST (STArray, STUArray, runSTArray, runSTUArray)
-- import Data.Array.Storable (StorableArray)
-- import Data.Array.Unboxed (UArray)
-- import Data.Bifunctor.Assoc
-- import Data.Bifunctor.Biff
-- import Data.Bifunctor.Clown
-- import Data.Bifunctor.Flip
-- import Data.Bifunctor.Join
-- import Data.Bifunctor.Joker
-- import Data.Bifunctor.Product
-- import Data.Bifunctor.Sum
-- import Data.Bifunctor.Swap
-- import Data.Bifunctor.Tannen
-- import Data.Can
--   ( Can (..),
--     can,
--     canCurry,
--     canEach,
--     canEachA,
--     canFst,
--     canSnd,
--     canUncurry,
--     canWithMerge,
--     codistributeCan,
--     distributeCan,
--     foldEnos,
--     foldOnes,
--     foldTwos,
--     isEno,
--     isNon,
--     isOne,
--     isTwo,
--     mapCans,
--     partitionCans,
--     type (⊗),
--   )
-- import Data.Can qualified
-- -- the same as from Control.Lens

-- import Data.Can qualified as Can
-- import Data.Crosswalk
-- import Data.Fix hiding (ana, anaM, cata, cataM, hylo, hyloM, refold)
-- import Data.Functor.Base hiding (head, tail)
-- --import Data.Functor.Combinator hiding (collectI, getI)
-- import Data.Functor.Foldable hiding (fold)
-- import Data.Functor.Product qualified as Product
-- import Data.Functor.Sum qualified as Sum
-- import Data.Functor.These
-- import Data.HashMap.Lazy qualified
-- import Data.IntMap qualified as IntMap
-- import Data.Ix
-- import Data.Map qualified as Map
-- import Data.Maybe qualified
-- import Data.Profunctor
-- import Data.Profunctor.Choice
-- import Data.Profunctor.Closed
-- import Data.Profunctor.Mapping
-- import Data.Profunctor.Ran
-- import Data.Profunctor.Rep
-- import Data.Profunctor.Sieve
-- import Data.Profunctor.Strong
-- import Data.Profunctor.Traversing
-- import Data.Profunctor.Yoneda
-- import Data.Semialign
-- import Data.Smash
--   ( Smash (..),
--     distributeSmash,
--     foldSmashes,
--     fromSmash,
--     hulkSmash,
--     isNada,
--     isSmash,
--     mapSmashes,
--     pairSmash,
--     pairSmashCan,
--     partitionSmashes,
--     quotSmash,
--     smash,
--     smashCurry,
--     smashDiag,
--     smashDiag',
--     smashFst,
--     smashSnd,
--     smashUncurry,
--     toSmash,
--     undistributeSmash,
--     unpairSmash,
--     unpairSmashCan,
--     type (⨳),
--   )
-- import Data.Smash qualified
-- import Data.Smash qualified as Smash
-- import Data.Tagged
-- import Data.These hiding (partitionHereThere, partitionThese)
-- import Data.These.Combinators hiding (assocThese, bimapThese, bitraverseThese, catHere, catThat, catThere, catThese, catThis, swapThese, unassocThese)
-- import Data.Time.Calendar
-- import Data.Time.Clock
-- import Data.Time.LocalTime
-- import Data.Traversable (for)
-- import Data.Validation hiding (fromEither, orElse, toEither)
-- import Data.Validation qualified as Validation
-- import Data.Wedge
--   ( Wedge (..),
--     codistributeWedge,
--     distributeWedge,
--     foldHeres,
--     foldTheres,
--     fromWedge,
--     isHere,
--     isNowhere,
--     isThere,
--     mapWedges,
--     partitionWedges,
--     quotWedge,
--     toWedge,
--     wedge,
--     wedgeLeft,
--     wedgeRight,
--     type (∨),
--   )
-- import Data.Wedge qualified
-- import Data.Wedge qualified as Wedge
-- import Data.Zip
-- import Relude.Applicative
-- import Relude.Base hiding (chr, ord)
-- import Relude.Bool
-- import Relude.Container hiding (swap)
-- import Relude.Debug
-- import Relude.DeepSeq
-- import Relude.Enum
-- import Relude.Exception
-- import Relude.File
-- import Relude.Foldable
-- import Relude.Function
-- import Relude.Functor hiding ((??))
-- import Relude.Lifted
-- import Relude.List hiding (filter, map, partitionWith, repeat, uncons, unzip, zip, zip3, zipWith, (++))
-- import Relude.Monad hiding (catMaybes, lefts, mapMaybe, mapMaybeM, partitionEithers, rights)
-- import Relude.Monoid hiding (Option, WrappedMonoid)
-- import Relude.Numeric
-- import Relude.Print
-- import Relude.String

-- type Biproduct = Data.Bifunctor.Product.Product

-- type Bisum = Data.Bifunctor.Sum.Sum

-- -- | Alias for 'fmap'
-- map :: Functor f => (a -> b) -> f a -> f b
-- map = fmap

-- -- | Alias for '(<>)'
-- (++) :: Monoid m => m -> m -> m
-- (++) = (<>)

-- -- mapIArray :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
-- -- mapIArray = IArray.amap

-- -- mapIIndices :: (IArray a e, Ix i, Ix j) => (i, i) -> (i -> j) -> a j e -> a i e
-- -- mapIIndices = IArray.ixmap

-- -- align :: Semialign f => f a -> f b -> f (These a b)

-- -- getIndices :: (MArray a e m, Ix i) => a i e -> m [i]
-- -- getIndices arr = do
-- --   b <- getBounds arr
-- --   case b of
-- --     (l, u) -> pure (range (l, u))

-- -- | Gets the enviroment
-- askW :: ComonadEnv e w => w a -> e
-- askW = Env.ask

-- -- | Gets the environment and applies the specified function
-- asksW :: ComonadEnv e w => (e -> e') -> w a -> e'
-- asksW = Env.asks

-- -- | Modifies the environment using the specified function
-- localW :: (e -> e') -> EnvT e w a -> EnvT e' w a
-- localW = Env.local

-- traceW :: ComonadTraced m w => m -> w a -> a
-- traceW = Trace.trace

-- -- -- TODO: overload?
-- -- zipperSize :: Zipper t a -> Int
-- -- zipperSize = StoreZipper.size

-- -- class Functor f => Filterable f where
-- --   {-# MINIMAL catMaybes | mapMaybe #-}

-- --   filterUnalign :: f (These a b) -> (f (Maybe a), f (Maybe b))
-- --   filterUnalign f = (fmap justHere f, fmap justThere f)

-- --   catMaybes :: f (Maybe a) -> f a
-- --   catMaybes = mapMaybe id

-- --   mapMaybe :: (a -> Maybe b) -> f a -> f b
-- --   mapMaybe f = catMaybes . map f

-- -- partitionEithers :: Filterable f => f (Either a b) -> (f a, f b)
-- -- partitionEithers = bimap catMaybes catMaybes . filterUnalign . map (either This That)

-- -- partitionWith :: Filterable f => (a -> Either b c) -> f a -> (f b, f c)
-- -- partitionWith f = partitionEithers . fmap f

-- -- filter :: Filterable f => (a -> Bool) -> f a -> f a
-- -- filter p = mapMaybe (\x -> if p x then Just x else Nothing)

-- -- lefts :: Filterable f => f (Either a b) -> f a
-- -- lefts = fst . partitionEithers

-- -- rights :: Filterable f => f (Either a b) -> f b
-- -- rights = snd . partitionEithers

-- -- (<$?>) :: Filterable f => (a -> Maybe b) -> f a -> f b
-- -- (<$?>) = mapMaybe

-- -- infixl 4 <$?>

-- -- (<&?>) :: Filterable f => f a -> (a -> Maybe b) -> f b
-- -- as <&?> f = mapMaybe f as

-- -- infixl 1 <&?>

-- -- partitionThese :: Filterable f => f (These a b) -> (f a, f b, f (a, b))
-- -- partitionThese f = (mapMaybe justThis f, mapMaybe justThat f, mapMaybe justThese f)

-- -- partitionHereThere :: Filterable f => f (These a b) -> (f a, f b)
-- -- partitionHereThere = bimap catMaybes catMaybes . filterUnalign

-- -- catThis :: Filterable f => f (These a b) -> f a
-- -- catThis = mapMaybe justThis

-- -- catThat :: Filterable f => f (These a b) -> f b
-- -- catThat = mapMaybe justThat

-- -- catThese :: Filterable f => f (These a b) -> f (a, b)
-- -- catThese = mapMaybe justThese

-- -- catHere :: Filterable f => f (These a b) -> f a
-- -- catHere = mapMaybe justHere

-- -- catThere :: Filterable f => f (These a b) -> f b
-- -- catThere = mapMaybe justThere

-- -- instance Filterable [] where
-- --   filterUnalign = foldr (these (\a -> first (Just a :)) (\b -> second (Just b :)) (\a b -> bimap (Just a :) (Just b :))) ([], [])
-- --   catMaybes = Data.Maybe.catMaybes

-- -- instance Filterable Maybe where
-- --   filterUnalign Nothing = (Nothing, Nothing)
-- --   filterUnalign (Just (This a)) = (Just (Just a), Just Nothing)
-- --   filterUnalign (Just (That b)) = (Just Nothing, Just (Just b))
-- --   filterUnalign (Just (These a b)) = (Just (Just a), Just (Just b))

-- --   catMaybes = join

-- -- instance Filterable IntMap where
-- --   mapMaybe = IntMap.mapMaybe

-- -- instance Filterable Seq where
-- --   filterUnalign = bimap fromList fromList . filterUnalign . toList
-- --   mapMaybe f = fromList . mapMaybe f . toList
-- --   catMaybes = fromList . catMaybes . toList

-- -- instance Monoid e => Filterable (Either e) where
-- --   mapMaybe f = \case
-- --     Left e -> Left e
-- --     Right a -> maybe (Left mempty) Right (f a)

-- -- instance Monoid e => Filterable (Validation e) where
-- --   mapMaybe f = \case
-- --     Failure e -> Failure e
-- --     Success a -> maybe (Failure mempty) Success (f a)

-- -- instance Filterable Proxy where
-- --   filterUnalign _ = (Proxy, Proxy)
-- --   catMaybes _ = Proxy
-- --   mapMaybe _ _ = Proxy

-- -- instance Filterable (Map k) where
-- --   mapMaybe = Map.mapMaybe

-- -- instance Functor f => Filterable (MaybeT f) where
-- --   mapMaybe f = MaybeT . fmap (mapMaybe f) . runMaybeT

-- -- instance Filterable ZipList where
-- --   filterUnalign = bimap ZipList ZipList . filterUnalign . getZipList
-- --   catMaybes = ZipList . catMaybes . getZipList
-- --   mapMaybe f = ZipList . mapMaybe f . getZipList

-- -- instance Filterable (HashMap k) where
-- --   mapMaybe = Data.HashMap.Lazy.mapMaybe

-- -- instance Filterable (Const r) where
-- --   filterUnalign (Const r) = (Const r, Const r)
-- --   catMaybes (Const r) = Const r
-- --   mapMaybe _ (Const r) = Const r

-- -- instance Filterable f => Filterable (IdentityT f) where
-- --   mapMaybe f (IdentityT a) = IdentityT (mapMaybe f a)

-- -- instance (Filterable f, Filterable g) => Filterable (Sum.Sum f g) where
-- --   mapMaybe f (Sum.InL x) = Sum.InL (mapMaybe f x)
-- --   mapMaybe f (Sum.InR x) = Sum.InR (mapMaybe f x)

-- -- instance (Filterable f, Filterable g) => Filterable (Product.Product f g) where
-- --   mapMaybe f (Product.Pair x y) = Product.Pair (mapMaybe f x) (mapMaybe f y)

-- -- instance (Functor f, Filterable g) => Filterable (Compose f g) where
-- --   mapMaybe f (Compose a) = Compose (fmap (mapMaybe f) a)

-- -- class (Traversable t, Filterable t) => Witherable t where
-- --   {-# MINIMAL #-}

-- --   wither :: (Witherable t, Applicative f) => (a -> f (Maybe b)) -> t a -> f (t b)
-- --   wither f = fmap catMaybes . traverse f

-- -- witherEither :: (Witherable t, Applicative f) => (a -> f (Either b c)) -> t a -> f (t b, t c)
-- -- witherEither f = fmap partitionEithers . traverse f

-- -- filterA :: (Witherable t, Applicative f) => (a -> f Bool) -> t a -> f (t a)
-- -- filterA f = wither (\x -> f x <&> \b -> if b then Just x else Nothing)

-- -- witherMap :: (Witherable t, Applicative m) => (t b -> r) -> (a -> m (Maybe b)) -> t a -> m r
-- -- witherMap p f = fmap p . wither f

-- -- eitherToValidation :: Either e a -> Validation e a
-- -- eitherToValidation = Validation.fromEither

-- -- validationToEither :: Validation e a -> Either e a
-- -- validationToEither = Validation.toEither

-- -- validateOrElse :: Validate v => v e a -> a -> a
-- -- validateOrElse = Validation.orElse

-- -- instance Witherable []

-- -- instance Witherable Maybe

-- -- instance Witherable IntMap

-- -- instance Witherable Seq

-- -- instance Monoid e => Witherable (Either e)

-- -- instance Monoid e => Witherable (Validation e)

-- -- instance Witherable Proxy

-- -- instance Witherable (Map k)

-- -- instance Traversable f => Witherable (MaybeT f)

-- -- instance Witherable ZipList

-- -- instance Witherable (HashMap k)

-- -- instance Witherable (Const r)

-- -- instance Witherable f => Witherable (IdentityT f)

-- -- instance (Witherable f, Witherable g) => Witherable (Sum.Sum f g)

-- -- instance (Witherable f, Witherable g) => Witherable (Product.Product f g)

-- -- instance (Witherable f, Witherable g) => Witherable (Compose f g)

-- -- instance Assoc Can where
-- --   assoc = Data.Can.reassocLR
-- --   unassoc = Data.Can.reassocRL

-- -- instance Swap Can where
-- --   swap = Data.Can.swapCan

-- -- instance Assoc Smash where
-- --   assoc = Data.Smash.reassocLR
-- --   unassoc = Data.Smash.reassocRL

-- -- instance Swap Smash where
-- --   swap = Data.Smash.swapSmash

-- -- instance Assoc Wedge where
-- --   assoc = Data.Wedge.reassocLR
-- --   unassoc = Data.Wedge.reassocRL

-- -- instance Swap Wedge where
-- --   swap = Data.Wedge.swapWedge

-- -- gatherCans :: (Zip f, Alternative f) => Can (f a) (f b) -> f (Can a b)
-- -- gatherCans Non = empty
-- -- gatherCans (One fs) = map One fs
-- -- gatherCans (Eno fs) = map Eno fs
-- -- gatherCans (Two fs gs) = zipWith Two fs gs

-- -- gatherWedges :: Alternative f => Wedge (f a) (f b) -> f (Wedge a b)
-- -- gatherWedges Nowhere = empty
-- -- gatherWedges (Here as) = fmap Here as
-- -- gatherWedges (There bs) = fmap There bs

-- -- gatherSmashes :: (Alternative f, Zip f) => Smash (f a) (f b) -> f (Smash a b)
-- -- gatherSmashes Nada = empty
-- -- gatherSmashes (Smash a b) = zipWith Smash a b

-- -- ones :: Filterable f => f (Can a b) -> f a
-- -- ones = mapMaybe \case
-- --   One a -> Just a
-- --   _ -> Nothing

-- -- enos :: Filterable f => f (Can a b) -> f b
-- -- enos = mapMaybe \case
-- --   Eno b -> Just b
-- --   _ -> Nothing

-- -- twos :: Filterable f => f (Can a b) -> f (a, b)
-- -- twos = mapMaybe \case
-- --   Two a b -> Just (a, b)
-- --   _ -> Nothing

-- -- heres :: Filterable f => f (Wedge a b) -> f a
-- -- heres = mapMaybe \case
-- --   Here a -> Just a
-- --   _ -> Nothing

-- -- theres :: Filterable f => f (Wedge a b) -> f b
-- -- theres = mapMaybe \case
-- --   There a -> Just a
-- --   _ -> Nothing

-- -- smashes :: Filterable f => f (Smash a b) -> f (a, b)
-- -- smashes = mapMaybe fromSmash

-- -- -- remove 'One's from a 'Filterable'
-- -- filterOnes :: Filterable f => f (Can a b) -> f (Can a b)
-- -- filterOnes = filter (not . isOne)

-- -- -- remove 'Eno's from a 'Filterable'
-- -- filterEnos :: Filterable f => f (Can a b) -> f (Can a b)
-- -- filterEnos = filter (not . isEno)

-- -- -- remove 'Two's from a 'Filterable'
-- -- filterTwos :: Filterable f => f (Can a b) -> f (Can a b)
-- -- filterTwos = filter (not . isTwo)

-- -- -- remove 'Non's from a 'Filterable'
-- -- filterNons :: Filterable f => f (Can a b) -> f (Can a b)
-- -- filterNons = filter (not . isNon)

-- -- filterHeres :: Filterable f => f (Wedge a b) -> f (Wedge a b)
-- -- filterHeres = filter (not . isHere)

-- -- filterTheres :: Filterable f => f (Wedge a b) -> f (Wedge a b)
-- -- filterTheres = filter (not . isThere)

-- -- filterNowheres :: Filterable f => f (Wedge a b) -> f (Wedge a b)
-- -- filterNowheres = filter (not . isNowhere)

-- -- filterNadas :: Filterable f => f (Smash a b) -> f (Smash a b)
-- -- filterNadas = filter (not . isNada)

-- -- partitionEitherToCan :: (Foldable f, Filterable f) => f (Either a b) -> Can (f a) (f b)
-- -- partitionEitherToCan f = case partitionEithers f of
-- --   (a, b) | null a && null b -> Non
-- --   (a, b) | null a -> Eno b
-- --   (a, b) | null b -> One a
-- --   (a, b) | otherwise -> Two a b

-- -- partitionSplitCans :: Filterable f => f (Can a b) -> (f a, f b, f (a, b))
-- -- partitionSplitCans f =
-- --   (\((a, b), c) -> (a, b, c)) $
-- --     first partitionEithers . partitionEithers $
-- --       flip
-- --         mapMaybe
-- --         f
-- --         \case
-- --           Non -> Nothing
-- --           One a -> Just $ Left $ Left a
-- --           Eno b -> Just $ Left $ Right b
-- --           Two a b -> Just $ Right (a, b)

-- -- class Unfolding c where
-- --   {-# MINIMAL unfoldingrM, iterateUntilM, accumUntilM #-}
-- --   unfoldingr :: Alternative f => (b -> c a b) -> b -> f a
-- --   unfoldingr f = runIdentity . unfoldingrM (pure . f)
-- --   unfoldingrM :: (Monad m, Alternative f) => (b -> m (c a b)) -> b -> m (f a)
-- --   iterateUntil :: Alternative f => (b -> c a b) -> b -> f a
-- --   iterateUntil f = runIdentity . iterateUntilM (pure . f)
-- --   iterateUntilM :: Monad m => Alternative f => (b -> m (c a b)) -> b -> m (f a)
-- --   accumUntil :: Alternative f => Monoid b => (b -> c a b) -> f a
-- --   accumUntil f = runIdentity (accumUntilM (pure . f))
-- --   accumUntilM :: Monad m => Alternative f => Monoid b => (b -> m (c a b)) -> m (f a)

-- -- instance Unfolding Either where
-- --   unfoldingrM f b =
-- --     f b >>= \case
-- --       Left a -> (pure a <|>) <$> unfoldingrM f b
-- --       Right b' -> unfoldingrM f b'
-- --   iterateUntilM f b =
-- --     f b >>= \case
-- --       Left a -> pure (pure a)
-- --       Right b' -> iterateUntilM f b'
-- --   accumUntilM f = go mempty
-- --     where
-- --       go b =
-- --         f b >>= \case
-- --           Left a -> (pure a <|>) <$> go b
-- --           Right b' -> go (b' `mappend` b)

-- -- instance Unfolding These where
-- --   unfoldingrM f b =
-- --     f b >>= \case
-- --       This a -> (pure a <|>) <$> unfoldingrM f b
-- --       That b' -> unfoldingrM f b'
-- --       These a b' -> (pure a <|>) <$> unfoldingrM f b'
-- --   iterateUntilM f b =
-- --     f b >>= \case
-- --       This a -> pure (pure a)
-- --       That b' -> iterateUntilM f b'
-- --       These a _ -> pure (pure a)
-- --   accumUntilM f = go mempty
-- --     where
-- --       go b =
-- --         f b >>= \case
-- --           This a -> (pure a <|>) <$> go b
-- --           That b' -> go (b' `mappend` b)
-- --           These a b' -> (pure a <|>) <$> go (b' `mappend` b)

-- -- instance Unfolding Can where
-- --   unfoldingrM = Can.unfoldrM
-- --   iterateUntilM = Can.iterateUntilM
-- --   accumUntilM = Can.accumUntilM

-- -- instance Unfolding Smash where
-- --   unfoldingrM = Smash.unfoldrM
-- --   iterateUntilM = Smash.iterateUntilM
-- --   accumUntilM = Smash.accumUntilM

-- -- instance Unfolding Wedge where
-- --   unfoldingrM = Wedge.unfoldrM
-- --   iterateUntilM = Wedge.iterateUntilM
-- --   accumUntilM = Wedge.accumUntilM

-- -- paraPlate :: Plated a => (a -> [r] -> r) -> a -> r
-- -- paraPlate = Control.Lens.para

-- -- universePlate :: Plated a => a -> [a]
-- -- universePlate = Control.Lens.universe

-- -- filterIndices ::
-- --   (Indexable i p, Applicative f) =>
-- --   (i -> Bool) ->
-- --   Optical' p (Indexed i) f a a
-- -- filterIndices = Control.Lens.indices

-- -- atIndex ::
-- --   (Indexable i p, Eq i, Applicative f) =>
-- --   i ->
-- --   Optical' p (Indexed i) f a a
-- -- atIndex = Control.Lens.index

-- -- -- critiques
-- -- -- Enum is non-total
-- -- -- File has locale-sensitive functions
-- -- -- FilePath is just wrong
-- -- -- Num hierarchy
-- -- -- cycle1

-- -- -- todo
-- -- -- module Relude.Nub via witherable
-- -- -- sorts, groups, nubs using discrimination
-- -- -- dropWhile etc using witherable
