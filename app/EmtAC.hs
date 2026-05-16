{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EmtAC where

import Control.Lens
import Data.Foldable (minimum)
import Data.IntMap qualified as IntMap
import Data.IntMap.Merge.Strict (dropMissing, mapMissing, merge, mergeA, preserveMissing, traverseMissing, zipWithAMatched, zipWithMatched)
import Egraph (EId, Egraph, unId)
import Lude

-- Map from EIds to exponent
type Monomial = IntMap Int

exponentAt :: EId -> Lens' Monomial Int
exponentAt e = at (e ^. unId) . non 0

data EMTAC f ann
  = EMTAC
  { _underlyingEgraph :: Egraph f ann,
    _acRW :: Map Monomial Monomial
  }

makeLenses ''EMTAC

-- Naive, non-incremental
handleMerge :: EId -> EId -> State (EMTAC f ann) ()
handleMerge a b = do
  let updExponents m =
        let (old, m') = m & exponentAt a <<.~ 0
         in m' & exponentAt b +~ old
  oldRW <- acRW <<.= mempty
  let ts = itoList oldRW <&> bimap updExponents updExponents
  -- should run until the list is empty
  _ <- extendingState' ts rebuild
  pass

rebuild :: State (EMTAC f ann, [(Monomial, Monomial)]) ()
rebuild =
  use _2 >>= \case
    [] -> pass
    ((l, r) : rest) -> do
      -- TODO: handle case when l and r are trivial (one entry) by deferring back
      -- to the Egraph
      _2 .= rest
      when (l /= r) do
        reducedL <- use (_1 . acRW) <&> ifoldl' (\l2 acc r2 -> snd $ reduceMon acc (l2, r2)) l
        reducedR <- use (_1 . acRW) <&> ifoldl' (\l2 acc r2 -> snd $ reduceMon acc (l2, r2)) r
        -- orient
        case compare reducedL reducedR of
          EQ -> pass
          LT -> handleCritical (reducedL, reducedR)
          GT -> handleCritical (reducedR, reducedL)

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
    _1 . acRW . at l2 ?= r1

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