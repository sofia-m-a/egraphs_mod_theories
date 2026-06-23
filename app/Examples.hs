{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Examples where

import Control.DeepSeq (NFData1)
import Control.Lens
import Control.Monad.Free (Free (..), iter)
import Control.Monad.ST (ST)
import Data.Fix (Fix (..))
import Data.Foldable1 (foldr1)
import Data.Functor.Classes (Show1)
import Data.GraphViz (GraphvizCanvas (Xlib), GraphvizOutput (Png, Svg), runGraphviz, runGraphvizCanvas')
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable (for)
import Egraph (EId (..), Egraph, Signature (..), edebug, eempty, eemptyWithMatcher, efindMatches, egsEnodes, einsert, einsertFree, ereannotate, esize, eunion, eunionInternal, prettyId)
import Ematch (MatchState, Matcher, Pattern, PatternVar, compilePatterns, convert, mdebug)
import GHC.Generics (Generic1, Generically, Generically1 (..))
import GraphDrawing
import Lude
import Prettyprinter
import Relude.Unsafe qualified
import Signature (NFSig)
import Test.QuickCheck
import Text.Show.Deriving

data Ex1 a
  = F a a
  | G a
  | H Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Signature Ex1 where
  type Symbol Ex1 = Ex1 ()
  type ACSymbol Ex1 = Void

example1 :: Egraph Ex1 Int
example1 = executingState
  (eempty 0 (\a b -> if a == b then (False, a) else (True, a `max` b)) example1Alg)
  do
    h1 <- einsert (H 3)
    h2 <- einsert (H 4)
    _ <- eunion h1 h2
    f1 <- einsert (F h1 h2)
    f2 <- einsert (F h1 h1)
    ereannotate f2 5
    pass

example1Dot :: DotGraph Text
example1Dot = toDot (Just show) example1Show example1

example1Viz :: IO ()
example1Viz = runGraphvizCanvas' example1Dot Xlib

example1Show :: Ex1 EId -> Text
example1Show (H i) = "H_" <> show i
example1Show (F (Id i) (Id j)) = "F(" <> show i <> ", " <> show j <> ")"
example1Show (G (Id i)) = "G(" <> show i <> ")"

example1Alg :: Ex1 Int -> Int
example1Alg (H i) = i
example1Alg (F i j) = i + j
example1Alg (G z) = -z

prettyEx :: Ex1 EId -> Doc ann
prettyEx (F a b) = "(F" <+> prettyId a <+> prettyId b <> ")"
prettyEx (G a) = "(G" <+> prettyId a <> ""
prettyEx (H i) = "(H" <+> viaShow i <> ")"

data Ex2 a = Ex2 [a] | Ex2C Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Signature Ex2 where
  type Symbol Ex2 = Either Text Int
  type ACSymbol Ex2 = ()

  symbolOf (Ex2 as) = Right (length as)
  symbolOf (Ex2C i) = Left i

  acSymbolOf (Ex2 _) = Just ()
  acSymbolOf (Ex2C _) = Nothing

  reconstruct (Right _) = Ex2
  reconstruct (Left i) = const (Ex2C i)

  reconstructAC _ = Ex2

var2 :: Text -> Free Ex2 EId
var2 = Free . Ex2C

list2 :: [Free Ex2 EId] -> Free Ex2 EId
list2 = Free . Ex2

trivialEmpty :: Egraph f ()
trivialEmpty = eempty () (\_ _ -> (False, ())) (const ())

example10 :: Egraph Ex2 ()
example10 =
  executingState
    trivialEmpty
    do
      a1 <- einsertFree (list2 [var2 "a", var2 "b", var2 "c"])
      a2 <- einsertFree (list2 [var2 "b", var2 "b"])
      a3 <- einsertFree (list2 [var2 "a", var2 "a", var2 "b", var2 "b"])
      eunion a1 a2

example10Dot :: DotGraph Text
example10Dot = toDot Nothing example10Show example10

example10Viz :: IO ()
example10Viz = runGraphvizCanvas' example10Dot Xlib

example1013Gen :: IO ()
example1013Gen = do
  _ <- runGraphviz (toDot Nothing example13Show example13) Svg "writings/buchexample.svg"
  _ <- runGraphviz example10Dot Svg "writings/buchexamplecomplete.svg"
  pass

example10Show :: Ex2 EId -> Text
example10Show (Ex2 as) = "+(" <> Text.intercalate ", " (fmap (\(Id i) -> show i) as) <> ")"
example10Show (Ex2C i) = i

data Ex2NonAC a = Ex2N [a] | Ex2NC Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Signature Ex2NonAC where
  type Symbol Ex2NonAC = Ex2NonAC ()
  type ACSymbol Ex2NonAC = Void

example13 :: Egraph Ex2NonAC ()
example13 =
  executingState
    trivialEmpty
    do
      let var3 = Free . Ex2NC
      a1 <- einsertFree (Free $ Ex2N [var3 "a", var3 "b", var3 "c"])
      a2 <- einsertFree (Free $ Ex2N [var3 "b", var3 "b"])
      a3 <- einsertFree (Free $ Ex2N [var3 "a", var3 "a", var3 "b", var3 "b"])
      eunion a1 a2

example13Show :: Ex2NonAC EId -> Text
example13Show (Ex2N as) = "+(" <> Text.intercalate ", " (fmap (\(Id i) -> show i) as) <> ")"
example13Show (Ex2NC i) = i

example13Viz :: IO ()
example13Viz = runGraphvizCanvas' (toDot Nothing example13Show example13) Xlib

data Ex3 a = Ex3Op a a | Ex3Var Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)

example3Show :: Ex3 EId -> Text
example3Show (Ex3Op (Id _a) (Id _b)) = "+"
example3Show (Ex3Var t) = t

-- apparently Generically1 is Eq1 but not Show1? Huh?
-- deriving (Show1) via Generically1 Ex3

deriveShow1 ''Ex3

instance Signature Ex3 where
  type Symbol Ex3 = Ex3 ()
  type ACSymbol Ex3 = Void

commonVars :: [Text]
commonVars = [fromList [c] | c <- ['a' .. 'z']]

associationsOf :: [a] -> [Free Ex3 a]
associationsOf [] = []
associationsOf [x] = [Pure x]
associationsOf (x : xs) = associationsOf xs >>= graft x
  where
    graft :: a -> Free Ex3 a -> [Free Ex3 a]
    graft x t = Free (Ex3Op (Pure x) t) : [Free (Ex3Op s z) | Free (Ex3Op y z) <- [t], s <- graft x y]

-- sanity check
catalanNumbers :: [Int]
catalanNumbers = [length (associationsOf [1 .. i]) | i <- [1 .. 10]] :: [Int]

exampleANaive :: Int -> Egraph Ex3 ()
exampleANaive n = executingState trivialEmpty do
  is <- for (take n commonVars) (einsert . Ex3Var)
  for_ (subsequences is) \js -> do
    case associationsOf js of
      [] -> pass
      t : ts -> do
        i <- einsertFree t
        for_ ts \t' -> do
          j <- einsertFree t'
          eunion i j

exampleACNaive :: Int -> Egraph Ex3 ()
exampleACNaive n = executingState trivialEmpty do
  is <- for (take n commonVars) (einsert . Ex3Var)
  for_ (subsequences is) \js -> do
    case concatMap associationsOf (permutations js) of
      [] -> pass
      t : ts -> do
        i <- einsertFree t
        -- Permutations?
        for_ ts \t' -> do
          j <- einsertFree t'
          eunion i j

exampleACNaiveSize :: Int -> Int
exampleACNaiveSize = view egsEnodes . esize . exampleACNaive

exampleACNaiveViz :: Int -> IO ()
exampleACNaiveViz i = runGraphvizCanvas' (toDot Nothing example3Show $ exampleACNaive i) Xlib

exampleACNaiveGen :: Int -> Int -> IO ()
exampleACNaiveGen i j = for_ ([i .. j] :: [Int]) \k -> runGraphviz (toDot Nothing example3Show $ exampleACNaive k) Svg ("writings/figures/blowup" ++ show k ++ ".svg")

exampleACNaiveGenPng :: Int -> Int -> IO ()
exampleACNaiveGenPng i j = for_ ([i .. j] :: [Int]) \k -> runGraphviz (toDot Nothing example3Show $ exampleACNaive k) Png ("writings/figures/blowup" ++ show k ++ ".png")

exampleNonWord :: Egraph Ex3 ()
exampleNonWord = executingState trivialEmpty do
  ea <- einsertFree (Free $ Ex3Var "a")
  eb <- einsertFree (Free $ Ex3Var "b")
  ec <- einsertFree (Free $ Ex3Var "c")
  ed <- einsertFree (Free $ Ex3Var "d")
  eab <- einsertFree (Free $ Ex3Op (Pure ea) (Pure eb))
  eaa <- einsertFree (Free $ Ex3Op (Pure ea) (Pure ea))
  ecb <- einsertFree (Free $ Ex3Op (Pure ec) (Pure eb))
  _ <- eunion eab eb
  _ <- eunion eaa ec
  _ <- eunion ecb ed
  pass

exampleNonWordGen :: IO ()
exampleNonWordGen = void $ runGraphviz (toDot Nothing example3Show exampleNonWord) Svg "writings/exampleNonWord.svg"

data Ex4 a
  = F4 a a
  | G4 a
  | H4 a a
  | K4 Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Signature Ex4 where
  type Symbol Ex4 = Ex4 ()
  type ACSymbol Ex4 = Void

example4Show :: Ex4 EId -> Text
example4Show (F4 _ _) = "f"
example4Show (G4 _) = "g"
example4Show (H4 _ _) = "h"
example4Show (K4 t) = t

g4f a = Free (G4 a)

f4f a b = Free (F4 a b)

h4f a b = Free (H4 a b)

a4 = Free (K4 "a")

b4 = Free (K4 "b")

c4 = Free (K4 "c")

example40 :: Egraph Ex4 ()
example40 = executingState trivialEmpty do
  _ <- einsertFree (h4f a4 (g4f b4))
  pass

example405 :: Egraph Ex4 ()
example405 = executingState trivialEmpty do
  _ <- einsertFree (f4f (g4f a4) c4)
  _ <- einsertFree (h4f a4 (g4f b4))
  pass

example41 :: Egraph Ex4 ()
example41 = executingState trivialEmpty do
  x1 <- einsertFree (f4f (g4f a4) c4)
  x2 <- einsertFree (h4f a4 (g4f b4))
  void (eunion x1 x2)

example42 :: Egraph Ex4 ()
example42 = executingState trivialEmpty do
  x1 <- einsertFree (f4f (g4f a4) c4)
  x2 <- einsertFree (h4f a4 (g4f b4))
  void (eunion x1 x2)
  y1 <- einsertFree (h4f (g4f c4) b4)
  y2 <- einsertFree (h4f a4 (g4f c4))
  void (eunion y1 y2)

example435 :: Egraph Ex4 ()
example435 = executingState trivialEmpty do
  x1 <- einsertFree (f4f (g4f a4) c4)
  x2 <- einsertFree (h4f a4 (g4f b4))
  void (eunion x1 x2)
  y1 <- einsertFree (h4f (g4f c4) b4)
  y2 <- einsertFree (h4f a4 (g4f c4))
  void (eunion y1 y2)
  z1 <- einsertFree (g4f b4)
  z2 <- einsertFree (g4f c4)
  void (eunionInternal z1 z2)

example43 :: Egraph Ex4 ()
example43 = executingState trivialEmpty do
  x1 <- einsertFree (f4f (g4f a4) c4)
  x2 <- einsertFree (h4f a4 (g4f b4))
  void (eunion x1 x2)
  y1 <- einsertFree (h4f (g4f c4) b4)
  y2 <- einsertFree (h4f a4 (g4f c4))
  void (eunion y1 y2)
  z1 <- einsertFree (g4f b4)
  z2 <- einsertFree (g4f c4)
  void (eunion z1 z2)

example44 :: Egraph Ex4 ()
example44 = executingState trivialEmpty do
  x1 <- einsertFree (f4f (g4f a4) c4)
  x2 <- einsertFree (h4f a4 (g4f b4))
  void (eunion x1 x2)
  y1 <- einsertFree (h4f (g4f c4) b4)
  y2 <- einsertFree (h4f a4 (g4f c4))
  void (eunion y1 y2)
  z1 <- einsertFree (g4f b4)
  z2 <- einsertFree (g4f c4)
  void (eunion z1 z2)
  w <- einsertFree (f4f (g4f a4) c4)
  void (eunion z1 w)

example45 :: Egraph Ex4 ()
example45 = executingState trivialEmpty do
  x1 <- einsertFree (f4f (g4f a4) c4)
  x2 <- einsertFree (h4f a4 (g4f b4))
  void (eunion x1 x2)
  y1 <- einsertFree (h4f (g4f c4) b4)
  y2 <- einsertFree (h4f a4 (g4f c4))
  void (eunion y1 y2)
  z1 <- einsertFree (g4f b4)
  z2 <- einsertFree (g4f c4)
  void (eunion z1 z2)
  -- w <- einsertFree (f4f (g4f a4) c4)
  -- void (eunion z1 w)
  for_ @[] [a4, b4, c4] $ \v -> do
    ea <- einsertFree v
    eb <- einsertFree (g4f v)
    void (eunion ea eb)

example4Viz :: Egraph Ex4 () -> IO ()
example4Viz i = runGraphvizCanvas' (toDot Nothing example4Show i) Xlib

example4Gen :: IO ()
example4Gen = do
  let examples = zip [1 ..] [example40, example405, example41, example42, example435, example43, example44, example45]
  for_ @[] examples \(i, e) -> runGraphviz (toDot Nothing example4Show e) Svg ("writings/" ++ "exampleBasic" ++ show i ++ ".svg")

debugMatches :: [(Int, EId, IntMap EId)] -> Doc ann
debugMatches ms =
  "Matches"
    <> line
    <> vsep
      ( fmap
          ( \(s, rt, subs) ->
              "Final state"
                <+> viaShow s
                <> line
                <> "Root node"
                <+> prettyId rt
                <> line
                <+> "Substitution"
                <> line
                <> indent 2 (vsep (fmap (\(pv, i) -> viaShow pv <+> "→" <+> prettyId i) (itoList subs)))
          )
          ms
      )

example5' :: Doc ann
example5' = (\(a, b) -> vsep [a, b]) $ bimap debugMatches (edebug viaShow viaShow viaShow) example5

example5'' :: Doc ann
example5'' =
  mdebug viaShow
    $ fst
    $ compilePatterns
    $ fromList
      [ (0, g4f (pure 0)),
        (1, f4f (pure 0) (pure 0)),
        (2, f4f (pure 0) (f4f (pure 1) (pure 2)))
      ]

example5 :: ([(Int, EId, IntMap EId)], Egraph Ex4 ())
example5 =
  let mat =
        convert
          $ fst
          $ compilePatterns
          $ fromList
            [ (0, g4f (pure 0)),
              (1, f4f (pure 0) (pure 0)),
              (2, f4f (pure 0) (f4f (pure 1) (pure 2)))
            ]
   in usingState (eemptyWithMatcher mat () (\_ _ -> (False, ())) (const ())) do
        x1 <- einsertFree (f4f a4 b4)
        ae <- einsertFree a4
        be <- einsertFree b4
        x2 <- einsertFree (g4f b4)
        x3 <- einsertFree (g4f c4)
        x4 <- einsertFree (f4f a4 (pure x1))
        _ <- eunion be x4
        eunion ae x1
        efindMatches

type SymNo = Int

type SymArity = Int

data ExampleBench a
  = BenchAC SymNo [a]
  | BenchSatAC SymNo a a
  | BenchF SymNo [a]
  | BenchConst Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)

deriveShow1 ''ExampleBench

instance (NFData a) => NFData (ExampleBench a)

instance NFData1 ExampleBench

instance NFSig ExampleBench

instance Signature ExampleBench where
  type Symbol ExampleBench = ExampleBench ()
  type ACSymbol ExampleBench = Int

  acSymbolOf (BenchAC i _) = Just i
  acSymbolOf _ = Nothing

  reconstructAC = BenchAC

exampleBenchShow :: ExampleBench EId -> Text
exampleBenchShow = \case
  BenchAC i _ -> "+_" <> show i
  BenchSatAC i _ _ -> "+_" <> show i
  BenchF i _ -> if 0 <= i && i < 26 then fromString [toEnum (fromEnum 'a' + i)] else "f_" <> show i
  BenchConst i -> show i

replicateNE :: Int -> a -> NonEmpty a
replicateNE n a = a :| replicate n a

replicateNEM :: (Monad m) => Int -> m a -> m (NonEmpty a)
replicateNEM n ma = sequence (replicateNE n ma)

smallerScale :: Int -> Gen a -> Gen a
smallerScale m a = do
  s <- getSize
  resize (if s < m then 0 else s - m) a

genTerm :: [(SymNo, SymArity)] -> Gen (Free ExampleBench Int)
genTerm i =
  if null i
    then Pure <$> chooseEnum (0, 3)
    else do
      (sn, sa) <- Test.QuickCheck.elements i
      s <- getSize
      Free . BenchF sn <$> replicateM sa (frequency [(4, Pure <$> chooseEnum (0, 3)), (min 1 s, smallerScale 1 (genTerm i))])

genACTerm :: SymNo -> [(SymNo, SymArity)] -> Gen (Free ExampleBench Int, Free ExampleBench Int)
genACTerm numACOps i = do
  size <- getSize
  acSym <- chooseEnum (0, numACOps - 1)
  len <- frequency [(4 + if size < 3 then 3 else 0, pure 2), (2, pure 3), (1, pure 4)]
  ts <- replicateNEM len (smallerScale 2 $ genTerm i)
  pure (foldr1 (\x y -> Free (BenchSatAC acSym x y)) ts, Free $ BenchAC acSym (toList ts))

data RwIndex
  = SatA SymNo
  | SatC SymNo
  | RandomRW Int
  deriving (Eq, Ord, Show)

type BenchTheory = (SymNo, [(SymNo, SymArity)], Matcher ExampleBench, MatchState -> IntMap EId -> Maybe (Free ExampleBench EId))

genBenchTheory :: Gen (BenchTheory, BenchTheory)
genBenchTheory = do
  numSyms <- chooseInt (0, 3)
  arities <- for [0 .. numSyms - 1] (\s -> (s,) <$> chooseInt (0, 3))

  numACOps <- chooseInt (1, 2)
  let acOps :: [(RwIndex, Pattern ExampleBench, Pattern ExampleBench)] =
        let x = Pure 0
            y = Pure 1
            z = Pure 2
         in concatMap @[]
              ( \i ->
                  [ ( SatA i,
                      Free (BenchSatAC i x (Free (BenchSatAC i y z))),
                      Free (BenchSatAC i (Free (BenchSatAC i x y)) z)
                    ),
                    ( SatC i,
                      Free (BenchSatAC i x y),
                      Free (BenchSatAC i y x)
                    )
                  ]
              )
              [0 .. numACOps - 1]

  numNonACOps <- if numSyms == 0 then pure 0 else chooseInt (0, 5)
  nonACOps <- for [0 .. numNonACOps - 1] \i -> do
    lhs <- genTerm arities
    rhs <- genTerm arities
    pure (RandomRW i, lhs, rhs)

  let finalAC1 =
        let ops = acOps ++ nonACOps
            (mb, stateMap) = compilePatterns (fromList (fmap (\(k, lhs, _rhs) -> (k, lhs)) ops))
            rhss = fromList @(Map _ _) $ fmap (\(k, _lhs, rhs) -> (k, rhs)) ops
            onMatch finalState finalSubs =
              stateMap
                ^. at finalState >>= \patK ->
                  rhss
                    ^. at patK >>= \rhs ->
                      traverse (\v -> finalSubs ^. at v) rhs
         in (numACOps, arities, convert mb, onMatch)

  let finalAC2 =
        let ops = nonACOps
            (mb, stateMap) = compilePatterns (fromList (fmap (\(k, lhs, _rhs) -> (k, lhs)) ops))
            rhss = fromList @(Map _ _) $ fmap (\(k, _lhs, rhs) -> (k, rhs)) ops
            onMatch finalState finalSubs =
              stateMap
                ^. at finalState >>= \patK ->
                  rhss
                    ^. at patK >>= \rhs ->
                      traverse (\v -> finalSubs ^. at v) rhs
         in (numACOps, arities, convert mb, onMatch)

  pure
    (finalAC1, finalAC2)

type InitialClasses = [NonEmpty (Fix ExampleBench)]

genInitialClasses :: SymNo -> [(SymNo, SymArity)] -> Gen (InitialClasses, InitialClasses)
genInitialClasses numACOps ars = do
  size <- getSize
  (nonACN, acN) <-
    if null ars
      then pure (0, size)
      else do
        spl <- chooseInt (0, size)
        pure (size - spl, spl)
  nonACs <-
    replicateM nonACN $ chooseInt (0, min 3 size) >>= \i -> smallerScale i $ replicateNEM i do
      t <- genTerm ars
      pure (iter Fix $ fmap (Fix . BenchConst) t)
  (acs1, acs2) <-
    fmap
      (unzip . fmap unzip)
      ( replicateM acN
          $ chooseInt (0, min 3 size)
          >>= \i -> smallerScale i $ replicateNEM i do
            (t1, t2) <- genACTerm numACOps ars
            pure (iter Fix $ fmap (Fix . BenchConst) t1, iter Fix $ fmap (Fix . BenchConst) t2)
      )
  pure (nonACs ++ acs1, nonACs ++ acs2)

