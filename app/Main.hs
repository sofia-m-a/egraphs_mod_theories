{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (evaluate)
import Control.Lens
import Control.Monad.Free
import Criterion.Main
import Data.Fix (Fix)
import Data.GraphViz (GraphvizCanvas (Xlib), runGraphvizCanvas', runGraphviz, GraphvizOutput (Svg))
import Egraph (EId, Egraph, EgraphStats, eemptyWithMatcher, efindMatches, egsACTotalSize, egsACnodes, egsEclasses, egsEnodes, einsertFix, einsertFree, esize, eunion)
import Ematch (MatchState)
import Examples (ExampleBench, InitialClasses, example10, exampleBenchShow, genBenchTheory, genInitialClasses)
import GraphDrawing (toDot)
import Lude
import System.Timeout (timeout)
import Test.QuickCheck (generate, resize)

main :: IO ()
main =
  defaultMain
    []

-- bgroup
--   "examples"
--   [ bench "example10" $ nf esize example10
--   ],
-- bgroup
--   "speed tests AC-SAT"
--   (fmap (\i -> bench ("speed bench size=" <> show i <> " AC-sat") $ speedBench i True) [2 .. 2]),
-- bgroup
--   "speed tests AC-Completion"
--   (fmap (\i -> bench ("speed bench size=" <> show i <> " AC-comp") $ speedBench i False) [2 .. 2])

-- speedBench :: Int -> Bool -> Benchmarkable
-- speedBench size satAC =
--   perRunEnv
--     (makeBench size satAC)
--     (pure . runBench 100)

-- sizeBench :: Int -> Bool -> IO EgraphStats
-- sizeBench size satAC = makeBench size satAC <&> runBench 1000

sizeComp :: Int -> IO (Int, Int)
sizeComp s = do
  (satT, nonSatT) <- generate (resize s genBenchTheory)
  (satEG, nonSatEG) <- generate $ resize s $ genInitialClasses (satT ^. _1) (satT ^. _2)

  let s1 = runBench 1000 (eemptyWithMatcher (satT ^. _3) () (\_ _ -> (False, ())) (const ()), satEG, satT ^. _4)
  let s2 = runBench 1000 (eemptyWithMatcher (nonSatT ^. _3) () (\_ _ -> (False, ())) (const ()), nonSatEG, nonSatT ^. _4)

  pure
    ( s1 ^. egsEclasses + s1 ^. egsEnodes + s1 ^. egsACnodes + s1 ^. egsACTotalSize,
      s2 ^. egsEclasses + s2 ^. egsEnodes + s2 ^. egsACnodes + s2 ^. egsACTotalSize
    )

sizeCompStats :: IO ()
sizeCompStats =
  replicateM
    10000
    ( timeout 1000000 do
        k <- sizeComp 4
        evaluateNF k
    )
    >>= print
    . catMaybes


makeBenchStart ::
  ( Egraph ExampleBench (),
    [NonEmpty (Fix ExampleBench)],
    MatchState -> IntMap EId -> Maybe (Free ExampleBench EId)
  ) ->
  Egraph ExampleBench ()
makeBenchStart (eg, init, _) = executingState eg $ for_ init \(t :| ts) -> do
  e <- einsertFix t
  for_ ts \t' -> do
    e' <- einsertFix t'
    eunion e' e

makeBenchExample :: Int -> IO ()
makeBenchExample s = do
  (satT, nonSatT) <- generate (resize s genBenchTheory)
  (satEG, nonSatEG) <- generate $ resize s $ genInitialClasses (satT ^. _1) (satT ^. _2)

  let s1 = makeBenchStart (eemptyWithMatcher (satT ^. _3) () (\_ _ -> (False, ())) (const ()), satEG, satT ^. _4)
  runGraphviz (toDot Nothing exampleBenchShow s1) Svg ("writings/" ++ "exampleRandom" ++ ".svg")
  pass

runBench ::
  Int ->
  ( Egraph ExampleBench (),
    [NonEmpty (Fix ExampleBench)],
    MatchState -> IntMap EId -> Maybe (Free ExampleBench EId)
  ) ->
  EgraphStats
runBench timeOutSat (eg, init, onmatch) =
  let eg' = executingState eg $ for_ init \(t :| ts) -> do
        e <- einsertFix t
        for_ ts \t' -> do
          e' <- einsertFix t'
          eunion e' e
   in evalState (satLoop timeOutSat) eg'
  where
    satLoop 0 = esize <$> get
    satLoop n =
      efindMatches >>= \mas ->
        if null mas
          then esize <$> get
          else do
            for_ mas \(s, r, subs) ->
              whenJust (onmatch s subs) \subsRhs' -> do
                einsertFree subsRhs' >>= void . eunion r
            satLoop (n - 1)