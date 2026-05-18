{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TermIndex where

import Control.Lens
import Data.Map qualified as Map
import Data.Map.Merge.Strict
import Data.Set qualified as Set
import Egraph (EId (..), Signature (..), unId)
import Lude

type PartialId = EId

data PartialTerm f
  = PartialTermC
  { _termComplete :: Maybe (f EId, EId),
    _termFurther :: Map EId PartialId
  }

makeLenses ''PartialTerm

data TermIndex f
  = TermIndexC
  { _tiBegin :: Map (Symbol f) PartialId,
    _tiStep :: Map PartialId (PartialTerm f),
    _tiUses :: Map EId (Set PartialId),
    _tiPartialU :: Map PartialId PartialId,
    _tiSortedPred :: Map PartialId EId,
    _tiNextP :: PartialId,
    _tiNextE :: EId,
    _tiDiscoveredEqs :: [(EId, EId)]
  }

makeLenses ''TermIndex

indexEmpty :: TermIndex f
indexEmpty = TermIndexC Map.empty Map.empty Map.empty Map.empty Map.empty (Id 0) (Id 0) []

indexStep :: PartialId -> Lens' (TermIndex f) (PartialTerm f)
indexStep p = tiStep . at p . anon (PartialTermC Nothing Map.empty) (\(PartialTermC c f) -> isNothing c && null f)

indexUses :: EId -> Lens' (TermIndex f) (Set PartialId)
indexUses e = tiUses . at e . anon Set.empty Set.null

indexAddInternal :: (Signature f) => f EId -> Symbol f -> [EId] -> Bool -> State (TermIndex f) (f EId, EId)
indexAddInternal f sym is doSorted = do
  b <-
    use (tiBegin . at sym) >>= \case
      Nothing -> tiNextP <<%= (unId +~ 1)
      Just p -> pure p
  go b is
  where
    go b [] = do
      PartialTermC o _ <- use (indexStep b)
      case o of
        Nothing -> do
          i <- tiNextE <<%= (unId +~ 1)
          indexStep b . termComplete ?= (f, i)
          pure (f, i)
        Just (f', i') -> pure (f', i')
    go b (e : es) = do
      PartialTermC _ n <- use (indexStep b)
      b' <- case n ^. at e of
        Nothing -> do
          b' <- tiNextP <<%= (unId +~ 1)
          indexUses e . contains b .= True
          when doSorted (tiSortedPred . at b' ?= e)
          pure b'
        Just b' -> pure b'
      go b' es

indexAdd :: (Signature f) => f EId -> State (TermIndex f) (f EId, EId)
indexAdd f = indexAddInternal f (symbolOf f) (toList f) False

indexAddSorted :: (Signature f) => f EId -> State (TermIndex f) (f EId, EId)
indexAddSorted f = indexAddInternal f (symbolOf f) (sort (toList f)) True

findPartial :: PartialId -> State (TermIndex f) PartialId
findPartial p =
  use (tiPartialU . at p) >>= \case
    Nothing -> pure p
    Just p' -> do
      p'' <- findPartial p'
      tiPartialU . at p ?= p''
      pure p''

mergeMap :: (Ord k) => Map k v -> Map k v -> ([(v, v)], Map k v)
mergeMap = mergeA preserveMissing preserveMissing (zipWithAMatched \_ v1 v2 -> ([(v1, v2)], v2))

mergeAt :: (Ord k) => k -> k -> Map k v -> (Maybe (v, v), Map k v)
mergeAt k1 k2 m =
  let (v1, m') = m & at k1 <<.~ Nothing in m' & at k2 %%~ mergeMaybe v1

mergeMaybe :: Maybe a -> Maybe a -> (Maybe (a, a), Maybe a)
mergeMaybe (Just v1) (Just v2) = (Just (v1, v2), Just v2)
mergeMaybe Nothing (Just v2) = (Nothing, Just v2)
mergeMaybe (Just v1) Nothing = (Nothing, Just v1)
mergeMaybe Nothing Nothing = (Nothing, Nothing)

updateUse :: (EId, EId) -> PartialId -> State (TermIndex f) ()
updateUse (c, r) p = do
  clash <- indexStep p . termFurther %%= mergeAt c r
  whenJust clash indexMergeP

indexMergeP :: (PartialId, PartialId) -> State (TermIndex f) ()
indexMergeP (c, r) = do
  c' <- findPartial c
  r' <- findPartial r
  when (c' /= r') do
    clash <- tiStep %%= mergeAt c' r'
    whenJust clash \(PartialTermC f1 m1, PartialTermC f2 m2) -> do
      let (dps, m) = mergeMap m1 m2
      let (de, f') = mergeMaybe f1 f2
      indexStep r' .= PartialTermC f' m
      whenJust de \((_, i1), (_, i2)) ->
        tiDiscoveredEqs <|= (i1, i2)
      for_ dps indexMergeP

indexMergeE :: (EId, EId) -> State (TermIndex f) ()
indexMergeE (c, r) = do
  usesC <- use (indexUses c)
  newUses <- for (toList usesC) \p -> do
    p' <- findPartial p
    pr <- use (tiSortedPred . at p')
    case pr of
      Nothing -> do
        updateUse (c, r) p
        pure p'
      Just e -> do
        -- o --e→ o --c→ o --...→
        --          --r→ o
        -- e ≤ c,
        -- c ≤ ...,
        -- e ≤ r
        nc <- use (indexStep p' . termFurther)
        let (mergeIntoR, mergeBeforeR) = Map.spanAntitone (<= r) nc
        _
  indexUses r <>= fromList newUses

indexSortSymbol :: (Signature f) => Symbol f -> State (TermIndex f) ()
indexSortSymbol sym = do
  s <- use (tiBegin . at sym)
  whenJust s go
  where
    go p = _
