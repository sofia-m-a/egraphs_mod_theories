module Examples where

import Control.Monad.Free (Free (..))
import Data.GraphViz.Types.Generalised (DotGraph)
import Egraph (EId (..), Egraph, Signature (..), eempty, einsert, einsertFree, ereannotate, eunion, prettyId)
import GraphDrawing
import Prettyprinter
import Data.GraphViz (runGraphvizCanvas', GraphvizCanvas (Gtk, Xlib))

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

data Ex2 a = Ex2 [a] | Ex2C Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Signature Ex2 where
  type Symbol Ex2 = Either Int Int
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

var2 :: Int -> Free Ex2 EId
var2 = Free . Ex2C

list2 :: [Free Ex2 EId] -> Free Ex2 EId
list2 = Free . Ex2

example10 :: Egraph Ex2 ()
example10 =
  executingState
    (eempty () (\_ _ -> (False, ())) (const ()))
    do
      a1 <- einsertFree (list2 [var2 0, var2 1, var2 2])
      a2 <- einsertFree (list2 [var2 1, var2 1])
      a3 <- einsertFree (list2 [var2 0, var2 0, var2 1, var2 1])
      eunion a1 a2

-- Old AC example. TODO
-- example1AC :: EMTAC Ex1 Int
-- example1AC = executingState (EMTAC eempty mempty) do
--   h1 <- zoom underlyingEgraph $ einsert example1Alg (H 3)
--   h2 <- zoom underlyingEgraph $ einsert example1Alg (H 4)
--   _ <- zoom underlyingEgraph $ eunion example1Alg h1 h2
--   f1 <- zoom underlyingEgraph $ einsert example1Alg (F h1 h2)
--   f2 <- zoom underlyingEgraph $ einsert example1Alg (F h1 f1)
--   zoom underlyingEgraph $ eannotate example1Alg f2 5
--   acRW . at (fromList [(h1 ^. unId, 2), (h2 ^. unId, 2)]) ?= fromList [(f1 ^. unId, 2)]
--   acRW . at (fromList [(h2 ^. unId, 3)]) ?= fromList [(f2 ^. unId, 2)]
--   -- x=y
--   -- x^2 y^2 → z^2
--   -- y^3 → w^2
--   --
--   -- y^3 → w^2
--   -- w^2 y → z^2
--   handleMerge example1Alg h1 h2
--   pass
