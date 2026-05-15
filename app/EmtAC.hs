{-# LANGUAGE TemplateHaskell #-}

module EmtAC where

import Control.Lens
import Data.IntMap qualified as IntMap
import Egraph (EId, Egraph (..), unId)
import Lude
import Data.Foldable (minimum)
import Data.IntMap.Merge.Strict (mergeA, dropMissing, traverseMissing, mapMissing, zipWithMatched, merge)

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
  oldRW <- acRW <<.= mempty
  ifor_ oldRW \l r -> do
    -- Uses simple ordering on EIds
    let (l', r') = if l < r then (l, r) else (r, l)
    use acRW >>= itraverse_ \l2 r2 -> do
      handleCritical (l', r') (l2, r2)
    _

data IntInf = Finite Int | Infinite
  deriving (Eq, Ord)
  
infDiv :: Int -> Int -> IntInf
infDiv a 0 = Infinite
infDiv a b = Finite (div a b)

reduceMon :: Monomial -> (Monomial, Monomial) -> Monomial
reduceMon x (l, r) = _
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
        Nothing -> x
        Just k -> if k == 0 then x else _

handleCritical :: (Monomial, Monomial) -> (Monomial, Monomial) -> State (EMTAC f ann) ()
handleCritical (l1, r1) (l2, r2) = do
  -- cases to handle:
  -- x^i in r1, x^0 (Nothing) in l2 → x^i is reducible by l2 (div i 0 is morally infinity: can reduce r1 by l2 as many times as necessary)
  -- x^0 (Nothing) in r1, x^i in l2 → can't reduce r1 by l2
  -- x^i in r1, x^j in l2 → can reduce r1 by at most div i j powers of l2
  let reducedR1 = mergeA dropMissing (traverseMissing \_ _ -> Nothing) _ r1 l2
  let multiplier = minimum r1
  let r1' = if multiplier > 0 then IntMap.unionWith () else _

  _