module Examples where

import Egraph (EId (..), Egraph, Signature (..), eempty, einsert, ereannotate, eunion, prettyId)
import Prettyprinter

data Ex1 a
  = F a a
  | G a
  | H Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Signature Ex1 where
  type Symbol Ex1 = Ex1 ()

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

example1Alg :: Ex1 Int -> Int
example1Alg (H i) = i
example1Alg (F i j) = i + j
example1Alg (G z) = -z

prettyEx :: Ex1 EId -> Doc ann
prettyEx (F a b) = "(F" <+> prettyId a <+> prettyId b <> ")"
prettyEx (G a) = "(G" <+> prettyId a <> ""
prettyEx (H i) = "(H" <+> viaShow i <> ")"

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
