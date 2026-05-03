{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Redundant lambda" -}

module UF
  ( EId (Id),
    unId,
    UF,
    Merge (..),
    usize,
    extend,
    union,
    union',
    ufind,
    siblings,
    partitions,
  )
where

import Control.Lens
import Data.Set qualified
import Prettyprinter (Doc, Pretty (..), comma, encloseSep, lbrace, rbrace, sep, viaShow, (<+>))

newtype EId = Id {_unId :: Int}
  deriving (Eq, Ord, Hashable, Show, Read, Generic)

makeWrapped ''EId

shiftId :: Int -> EId -> EId
shiftId i (Id j) = Id (i + j)

instance Bounded EId where
  minBound = Id 0
  maxBound = Id maxBound

makeLenses ''EId

newtype UFRoot = UFRoot {_ufRank :: Int}
  deriving (Eq, Ord, Show)

makeLenses ''UFRoot

data UFNode
  = Leaf !EId
  | Root !UFRoot
  deriving (Show)

makePrisms ''UFNode

-- TODO cleanups:
-- split sibling structure out
-- pretty up siblings/partitions
-- create a version of partitions that doesn't use siblings

-- An ID updated in a merge
newtype Merge = MergeC EId
  deriving (Eq, Ord, Show, Read, Generic)

data UF = UFC
  { _canon :: IntMap UFNode,
    _sibling :: IntMap EId,
    _nextEId :: !EId
  }
  deriving (Show)

makeLenses ''UF

instance Semigroup UF where
  UFC c s (Id i) <> UFC d t j = UFC (c <> d') (s <> t') (shiftId i j)
    where
      d' = fromList (bimap (i +) (_Leaf %~ shiftId i) <$> itoList d)
      t' = fromList (bimap (i +) (shiftId i) <$> itoList t)

instance Monoid UF where
  mempty = UFC mempty mempty (Id 0)

canon' :: EId -> Lens' UF (Maybe UFNode)
canon' eid = canon . at (eid ^. unId)

usize :: UF -> Int
usize = view (nextEId . unId . to (subtract 1))

-- | Extends the union find, returning a fresh EId
extend :: UF -> (EId, UF)
extend = nextEId <<%~ shiftId 1

-- findInternal by simplified loop
findInternal :: EId -> UF -> ((EId, UFRoot), UF)
findInternal e uf = h e
  where
    h = \a -> case uf ^. canon' a of
      Nothing -> let r = UFRoot 0 in ((a, r), uf & canon' a ?~ Root r)
      Just (Root r) -> ((a, r), uf)
      Just (Leaf (h -> (root@(rootName, _), uf'))) -> (root, uf' & canon' a ?~ Leaf rootName)

-- findInternal by explicit refold recursion scheme
-- findInternal :: EId -> UF -> ((EId, UFRoot), UF)
-- findInternal e uf =
--   refold @(CofreeF (Either _) _)
--     ( \(e' Control.Comonad.Trans.Cofree.:< nx) -> case nx of
--         Left r -> ((e', r), uf & canon . at (e' ^. unId) ?~ Root r)
--         Right ((e'', r), uf) -> ((e'', r), uf & canon . at (e'' ^. unId) ?~ Leaf e'')
--     )
--     ( \a ->
--         a Control.Comonad.Trans.Cofree.:< case uf ^. canon . at (a ^. unId) of
--           Nothing -> Left (UFRoot 0)
--           Just (Root r) -> Left r
--           Just (Leaf a') -> Right a'
--     )
--     e

-- findInternal by stateful muck
-- findInternal :: EId -> UF -> ((EId, UFRoot), UF)
-- findInternal = runState . ufFindState
--   where
--     ufFindState a = do
--       s <- use (canon . at (a ^. unId))
--       case s of
--         Just (Leaf s') -> do
--           (p, r) <- ufFindState s'
--           canon . at (a ^. unId) ?= Leaf p
--           pure (p, r)
--         Just (Root r) -> pure (a, r)
--         Nothing -> do
--           let newRoot = UFRoot 0
--           canon . at (a ^. unId) ?= Root newRoot
--           pure (a, newRoot)

ufind :: EId -> UF -> (EId, UF)
ufind a uf = findInternal a uf & _1 %~ view _1

union' :: EId -> EId -> UF -> ((EId, Merge), UF)
union' a b = runState $ do
  (a', ar) <- state (findInternal a)
  (b', br) <- state (findInternal b)
  -- Decide the order in which to union
  let (x1, x2, newR, mr) = case compare ar br of
        LT -> (a', b', br, MergeC a')
        EQ -> (a', b', br & ufRank +~ 1, MergeC a')
        GT -> (b', a', ar, MergeC b')
  -- Do the union
  canon . at (x1 ^. unId) ?= Leaf x2
  canon . at (x2 ^. unId) ?= Root newR
  -- Connect the sibling cycles
  s1 <- use (sibling . at (x1 ^. unId))
  s2 <- use (sibling . at (x2 ^. unId))
  sibling . at (x1 ^. unId) ?= fromMaybe x2 s2
  sibling . at (x2 ^. unId) ?= fromMaybe x1 s1
  pure (x2, mr)

union :: EId -> EId -> UF -> (EId, UF)
union a b uf = let ((r, _), uf') = union' a b uf in (r, uf')

siblings :: EId -> UF -> (Set EId, UF)
siblings a = runState $ collectSiblings a (one a)
  where
    collectSiblings n sibs = do
      n' <- use (sibling . at (n ^. unId))
      case n' of
        Just n'' | n'' /= a -> collectSiblings n'' (sibs <> one n'')
        _ -> pure sibs

partitions :: UF -> ([(EId, Set EId)], UF)
partitions = runState $ do
  n <- use nextEId
  go (Id 0) n mempty
  where
    go s e _sk | s >= e = pure []
    go s e sk | sk ^. contains s = go (s & unId +~ 1) e sk
    go s e sk = do
      sibs <- state (siblings s)
      ((s, sibs) :) <$> go (s & unId +~ 1) e (sk <> sibs)

instance Pretty UF where
  pretty uf =
    sep
      ( (\(r, cl) -> mkId r <> ":" <+> encloseSep lbrace rbrace comma (fmap mkId (Data.Set.toAscList cl)))
          <$> fst (partitions uf)
      )
    where
      mkId (Id n) = "e" <> viaShow n

exampleE :: UF
exampleE = executingState mempty $ do
  e0 <- state extend
  e1 <- state extend
  state (union e0 e1)
  e2 <- state extend
  e3 <- state extend
  state (union e2 e0)
  e4 <- state extend
  e5 <- state extend
  state (union e5 e4)
  e6 <- state extend
  pass

exampleD :: Doc ann
exampleD = pretty exampleE