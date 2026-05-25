{-# LANGUAGE TemplateHaskell #-}

module Examples where

import Control.Monad.Free (Free (..))
import Data.Functor.Classes (Eq1, Show1)
import Data.GraphViz (GraphvizCanvas (Xlib), GraphvizOutput (Png, Svg, XDot), runGraphviz, runGraphvizCanvas')
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.Text qualified as Text
import Data.Traversable (for)
import Egraph (EId (..), Egraph, Signature (..), eempty, einsert, einsertFree, ereannotate, eunion, eunionInternal, prettyId)
import GHC.Generics (Generic1, Generically, Generically1 (..))
import GraphDrawing
import Prettyprinter
import Text.Show.Deriving

data Ex1 a
  = F a a
  | G a
  | H Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Signature Ex2 where
  type Symbol Ex2 = Either Text Int
  type ACSymbol Ex2 = ()

  symbolOf (Ex2 as) = Right (length as)
  symbolOf (Ex2C i) = Left i

  acSymbolOf (Ex2 _) = Just ()
  acSymbolOf (Ex2C _) = Nothing

  arity (Ex2 as) = length as
  arity (Ex2C _) = 0

  arity' _ = fromRight 0

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

exampleACNaive :: Int -> Egraph Ex3 ()
exampleACNaive n = executingState trivialEmpty do
  is <- for (take n commonVars) (einsert . Ex3Var)
  for_ (subsequences is) \js -> do
    case associationsOf js of
      [] -> pass
      t : ts -> do
        i <- einsertFree t
        for_ ts \t' -> do
          j <- einsertFree t'
          eunion i j

exampleACNaiveViz :: Int -> IO ()
exampleACNaiveViz i = runGraphvizCanvas' (toDot Nothing example3Show $ exampleACNaive i) Xlib

exampleACNaiveGen :: Int -> Int -> IO ()
exampleACNaiveGen i j = for_ ([i .. j] :: [Int]) \k -> do
  runGraphviz (toDot Nothing example3Show $ exampleACNaive k) Svg ("writings/blowup" ++ show k ++ ".svg")

exampleACNaiveGenPng :: Int -> Int -> IO ()
exampleACNaiveGenPng i j = for_ ([i .. j] :: [Int]) \k -> do
  runGraphviz (toDot Nothing example3Show $ exampleACNaive k) Png ("writings/blowup" ++ show k ++ ".png")

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

example4Viz :: Egraph Ex4 () -> IO ()
example4Viz i = runGraphvizCanvas' (toDot Nothing example4Show i) Xlib

example4Gen :: IO ()
example4Gen = do
  let examples = zip [1 ..] [example40, example405, example41, example42, example435, example43, example44]
  for_ examples \(i, e) -> do
    runGraphviz (toDot Nothing example4Show e) Svg ("writings/" ++ "exampleBasic" ++ show i ++ ".svg")