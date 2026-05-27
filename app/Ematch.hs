{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ematch (Matcher, mdebug, matcherEmpty, matcherFinal, MatchState, matcherStart, MatcherAnn, stepEmatcher, mergeEmatcher, compilePatterns, convert) where

import Control.Lens
import Control.Monad.Free (Free (..), foldFree, iterA)
import Data.Functor.Foldable (Recursive (cata))
import Data.IntMap.Merge.Strict (preserveMissing, zipWithAMatched)
import Data.IntMap.Merge.Strict qualified as IntMap
import Data.IntMap.Monoidal (MonoidalIntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lude
import Prettyprinter
import Signature

type PatternVar = Int

type Pattern f = Free f PatternVar

type MatchState = Int

data MatcherBuilder f
  = MatcherBuilderC
  { _matcherBNextState :: MatchState,
    -- e@{} → e@{(s, {v → e})} for each relevant v
    _matcherBStart :: Map PatternVar MatchState,
    -- f(..e_i..) → e, e_i@{(s_i, subs_i)}   ===>   e@{(s, join_i(subs_i)}
    _matcherBStep :: Map (f MatchState) MatchState,
    -- final states
    _matcherBFinal :: IntSet
  }

makeLenses ''MatcherBuilder

data Matcher f
  = MatcherC
  { _matcherStart :: Set (MatchState, IntMap ()),
    _matcherStep :: Map (f MatchState) MatchState,
    _matcherFinal :: IntSet
  }

makeLenses ''Matcher

matcherEmpty :: Matcher f
matcherEmpty = MatcherC Set.empty Map.empty IntSet.empty

convert :: MatcherBuilder f -> Matcher f
convert m =
  MatcherC
    (fromList $ fmap (\(v, s) -> (s, one (v, ()))) $ itoList $ m ^. matcherBStart)
    (m ^. matcherBStep)
    (m ^. matcherBFinal)

compilePatterns :: (Signature f, Ord (f Int)) => [Pattern f] -> MatcherBuilder f
compilePatterns pats = executingState (MatcherBuilderC 0 Map.empty Map.empty IntSet.empty) $ for_ pats \pat -> do
  outState <-
    pat
      -- Assign states to variable captures
      & traverse
        ( \pv ->
            use (matcherBStart . at pv) >>= flip whenNothing do
              i <- matcherBNextState <<+= 1
              matcherBStart . at pv ?= i
              pure i
        )
        -- Traverse the pattern as a tree, assigning steps/states
        >>= iterA
          ( \f -> do
              f' <- sequence f
              i <- matcherBNextState <<+= 1
              matcherBStep . at f' ?= i
              pure i
          )
  matcherBFinal . contains outState .= True

mdebug :: (f MatchState -> Doc ann) -> MatcherBuilder f -> Doc ann
mdebug showNode m =
  "Matcher with"
    <+> viaShow (m ^. matcherBNextState)
    <+> "states:"
    <> line
    <> indent
      2
      ( vsep
          [ "Null states"
              <> indent
                2
                ( vsep
                    $ fmap
                      ( \(pv, s) ->
                          "Pattern variable" <+> viaShow pv <+> "captured by state" <+> viaShow s
                      )
                    $ itoList (m ^. matcherBStart)
                ),
            "Step states"
              <> indent
                2
                (vsep $ fmap (\(n, s) -> showNode n <+> "→" <+> viaShow s) $ itoList (m ^. matcherBStep)),
            "Final states"
              <> indent
                2
                (list $ fmap viaShow $ IntSet.toList (m ^. matcherBFinal))
          ]
      )

joinSubs :: [IntMap EId] -> Maybe (IntMap EId)
joinSubs =
  foldl'
    ( \m n ->
        m
          >>= IntMap.mergeA
            preserveMissing
            preserveMissing
            (zipWithAMatched (\_ a b -> guard (a == b) $> a))
            n
    )
    (pure IntMap.empty)

mergeSubsLists :: [IntMap EId] -> State [IntMap EId] Bool
mergeSubsLists ss = do
  old <- fromList @(Set _) <$> get
  let added = executingState (False, old) $ for_ ss \s ->
        use (_2 . contains s) >>= \b -> unless b do
          _2 . contains s .= True
          _1 .= True
  put (toList $ snd added)
  pure (fst added)

type MatcherAnn = IntMap [IntMap EId]

stepEmatcher :: (Signature f) => Matcher f -> f MatcherAnn -> State MatcherAnn Bool
stepEmatcher m en = do
  l <- for (itoList (m ^. matcherStep)) \(sig, s) -> do
    if symbolOf sig == symbolOf en
      then for (zipWithM (\s ss -> ss ^. at s) (toList sig) (toList en)) \subs -> do
        let prod = mapMaybe joinSubs $ sequence subs
        zoom (at s . anon [] null) (mergeSubsLists prod)
      else pure Nothing
  pure (Just True `elem` l)

mergeEmatcher :: Matcher f -> MatcherAnn -> MatcherAnn -> (Bool, MatcherAnn)
mergeEmatcher m =
  (first getAny .)
    . IntMap.mergeA
      preserveMissing
      preserveMissing
      (zipWithAMatched $ \_ a b -> first Any $ runState (mergeSubsLists a) b)