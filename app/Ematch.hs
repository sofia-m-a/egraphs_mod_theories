{-# LANGUAGE TemplateHaskell #-}

module Ematch where

import Control.Lens
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Egraph
import Lude
import Data.IntMap.Monoidal (MonoidalIntMap)

data Epattern enode v
  = JoinP (Seq (enode v))
  deriving (Functor)

data EpatternList f v = EpatternList { _patterns :: Seq (Epattern f v)}
makeLenses ''EpatternList

data MatchState v = MatchState
  { _msIndex :: Int
  , _msSubsts :: Seq (Map v EId)
  }
makeLenses ''MatchState

-- instance Semigroup (MatchState v) where
--   MatchState a b <> MatchState c d = MatchState (max a b) ()

data EmatchStateAnn f v = EmatchState
  { -- Pattern → (index, current subst)
    _matchState :: IntMap (MatchState v)
  }
makeLenses ''EmatchStateAnn

-- instance CSL (EmatchStateAnn f v) where
--   type Delta = MonoidalIntMap (Max Int)

-- data EmatchState v enode
--   = EmatchStateC
--   { _eMatch :: Map v EId
--   }

-- makeLenses ''EmatchState

-- data AnnEgraph ann enode
--   = AE
--   { _base :: Egraph enode,
--     _ann :: IntMap ann,
--     _annMerge :: ann -> ann -> Maybe ann,
--     _annF :: enode ann -> ann -> Maybe ann
--   }

-- makeLenses ''AnnEgraph
-- aeFind :: EId -> Lens' (AnnEgraph ann enode) EId
-- aeFind e = base . efind e

-- aePropagate :: (EId, EId) -> State (AnnEgraph enode ann) [(EId, EId)]
-- aePropagate (a, b) = do
--   m <- use annMerge 
--   xa <- use (ann . at (a ^. unId))
--   xb <- use (ann . at (b ^. unId))
--   ps <- zoom base (epropagate (a, b))
--   ab <- use (base . efind a)
--   ann . at (a ^. unId) .= Nothing
--   ann . at (b ^. unId) .= Nothing
--   ann . at (ab ^. unId) .= m xa xb
--   _
