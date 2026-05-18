{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EmtAC where

import Control.Lens
import Data.Foldable (minimum)
import Data.IntMap qualified as IntMap
import Data.IntMap.Merge.Strict (dropMissing, mapMissing, merge, mergeA, preserveMissing, traverseMissing, zipWithAMatched, zipWithMatched)
import Egraph (CSL, EId (..), Egraph, Ex1 (..), Signature, eannotate, edebug, eempty, einsert, eunion, example1Alg, unId, prettyEx)
import Lude
import Prettyprinter (Doc, indent, list, viaShow, vsep, (<+>))

-- Map from EIds to exponent
type Monomial = IntMap Int

grevlex :: Monomial -> Monomial -> Ordering
grevlex t u = comparing cmp t u
    where
      cmp r = (sum r, map negate (degrees r)) -- grevlex order

      vars = sort (IntMap.keys t ++ IntMap.keys u)
      degrees r = [IntMap.findWithDefault 0 x r | x <- vars]

exponentAt :: EId -> Lens' Monomial Int
exponentAt e = at (e ^. unId) . non 0

data EMTAC f ann
  = EMTAC
  { _underlyingEgraph :: Egraph f ann,
    _acRW :: Map Monomial Monomial
  }

makeLenses ''EMTAC

eacDebug :: (a -> Doc ann) -> (f EId -> Doc ann) -> EMTAC f a -> Doc ann
eacDebug showAnn showNode e =
  vsep
    [ edebug showAnn showNode (e ^. underlyingEgraph),
      "with AC basis",
      indent
        2
        (vsep ((\(lhs, rhs) -> prettyMon lhs <+> "→" <+> prettyMon rhs) <$> itoList (e ^. acRW)))
    ]
  where
    prettyMon :: Monomial -> Doc ann
    prettyMon m | null m = "1"
    prettyMon m = list . fmap (\(x, i) -> if i == 1 then viaShow x else viaShow x <> "^" <> viaShow i) . itoList $ m

example1AC :: EMTAC Ex1 Int
example1AC = executingState (EMTAC eempty mempty) do
  h1 <- zoom underlyingEgraph $ einsert example1Alg (H 3)
  h2 <- zoom underlyingEgraph $ einsert example1Alg (H 4)
  _ <- zoom underlyingEgraph $ eunion example1Alg h1 h2
  f1 <- zoom underlyingEgraph $ einsert example1Alg (F h1 h2)
  f2 <- zoom underlyingEgraph $ einsert example1Alg (F h1 f1)
  zoom underlyingEgraph $ eannotate example1Alg f2 5
  acRW . at (fromList [(h1 ^. unId, 2), (h2 ^. unId, 2)]) ?= fromList [(f1 ^. unId, 2)]
  acRW . at (fromList [(h2 ^. unId, 3)]) ?= fromList [(f2 ^. unId, 2)]
  -- x=y
  -- x^2 y^2 → z^2
  -- y^3 → w^2
  --
  -- y^3 → w^2
  -- w^2 y → z^2
  handleMerge example1Alg h1 h2
  pass

-- Naive, non-incremental
handleMerge :: (Signature f, CSL ann) => (f ann -> ann) -> EId -> EId -> State (EMTAC f ann) ()
handleMerge anno a b = do
  let updExponents m =
        let (old, m') = m & exponentAt a <<.~ 0
         in m' & exponentAt b +~ old
  oldRW <- acRW <<.= mempty
  let ts = itoList oldRW <&> bimap updExponents updExponents
  -- should run until the list is empty
  _ <- extendingState' ts (rebuild anno)
  pass

rebuild :: (Signature f, CSL ann) => (f ann -> ann) -> State (EMTAC f ann, [(Monomial, Monomial)]) ()
rebuild anno =
  use _2 >>= \case
    [] -> pass
    ((l, r) : rest) -> do
      _2 .= rest
      reducedL <- use (_1 . acRW) <&> ifoldl' (\l2 acc r2 -> snd $ reduceMon acc (l2, r2)) l
      reducedR <- use (_1 . acRW) <&> ifoldl' (\l2 acc r2 -> snd $ reduceMon acc (l2, r2)) r
      traceShowM (l, r, reducedL, reducedR)
      when (reducedL /= reducedR) do
        -- handle case when l and r are trivial (one entry) by deferring back
        -- to the Egraph
        case (itoList reducedL, itoList reducedR) of
          ([(x, 1)], [(y, 1)]) -> do
            _ <- zoom (_1 . underlyingEgraph) (eunion anno (Id x) (Id y))
            pass
          _ -> do
            -- orient
            case grevlex reducedL reducedR of
              EQ -> pass
              GT -> handleCritical (reducedL, reducedR)
              LT -> handleCritical (reducedR, reducedL)
      rebuild anno

-- l1 and r1 is reduced WRT all the monomials in acRW
-- We need to see if any of them are reducible WRT it
-- and find critical pairs.
-- Note: if we have l2 → r2 reducible by l1 → r1,
-- then this is a critical pair between l1 and l2 that is simply l2 itself
-- So the extra case to handle is that r2 is reducible by l1
handleCritical :: (Monomial, Monomial) -> State (EMTAC f ann, [(Monomial, Monomial)]) ()
handleCritical (l1, r1) = do
  use (_1 . acRW) >>= itraverse_ \l2 r2 -> do
    let (didReduce, r2') = reduceMon r2 (l1, r1)
    when didReduce do
      _1 . acRW . at l2 ?= r2'
    let crit = criticalPair l1 l2
    whenJust crit \crit' -> do
      let (_, crit1) = reduceMon crit' (l1, r1)
      let (_, crit2) = reduceMon crit' (l2, r2)
      _2 <|= (crit1, crit2)
  _1 . acRW . at l1 ?= r1

criticalPair :: Monomial -> Monomial -> Maybe Monomial
criticalPair a b =
  let (anyNonTrivial, criticalTerm) =
        mergeA
          (traverseMissing (\_ ia -> (Any False, ia)))
          (traverseMissing (\_ ib -> (Any False, ib)))
          (zipWithAMatched (\_ ia ib -> (Any True, max ia ib)))
          a
          b
   in if getAny anyNonTrivial then Just criticalTerm else Nothing

reduceMon :: Monomial -> (Monomial, Monomial) -> (Bool, Monomial)
reduceMon x (l, r) =
  -- Step 1. Find the maximum k such that x is divisible by l^k
  -- x^i in r1, x^0 in l2 → no constraints on k
  -- x^i in r1, x^j in l2 → k can be at most div i j
  -- x^0 in r1, x^j in l2 → k can be at most 0 (special case of the above)
  -- (technically, the first case is also a special case if we consider the partially ordered set N union infinity)
  -- Q: could be optimized by mergeA (make the third case go to Nothing, to shortcut
  -- when one monomial is not divisible by the other)
  let maxKMap = merge dropMissing (mapMissing \_ _ -> 0) (zipWithMatched \_ a b -> div a b) x l
      -- the maximum k is bounded by each of the divisors above... just to say yes,
      -- this is supposed to be the minimum
      maxK = if null maxKMap then Nothing else Just $ minimum maxKMap
   in case maxK of
        -- Example of this case: x = x_1 x_2, l = 1
        -- Clearly, x is always divisible by l^k no matter the k
        -- In fact, this case only happens when l is 1.
        -- The rewrite rule 1 → r for some r is a bit ill-behaved,
        -- theoretically we 'should' rewrite x to x r^infinity
        -- But we will just pretend nothing happened and hope this case never occurs
        Nothing -> (False, x)
        Just k ->
          if k == 0
            then (False, x)
            else
              let x_div_l_k = merge preserveMissing dropMissing (zipWithMatched \_ a b -> a - k * b) x l
               in (True, filter (> 0) $ IntMap.unionWith (+) x_div_l_k (fmap (* k) r))