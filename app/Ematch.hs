{-# LANGUAGE TemplateHaskell #-}

module Ematch where

import Control.Lens
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Egraph
import Lude

data Epattern enode v
  = JoinP [enode v]
  deriving (Functor)

data EmatchState v enode
  = EmatchStateC
  { _eMatch :: Map v EId
  }

makeLenses ''EmatchState

data AnnEgraph ann enode
  = AE
  { _base :: Egraph enode,
    _ann :: IntMap ann,
    _annMerge :: ann -> ann -> Maybe ann,
    _annF :: enode ann -> ann -> Maybe ann
  }

makeLenses ''AnnEgraph
aeFind :: EId -> Lens' (AnnEgraph ann enode) EId
aeFind e = base . efind e

aePropagate :: EId -> EId -> State (AnnEgraph enode ann) EId
aePropagate a b = do
  xa <- use (ann . at (a ^. unId))
  xb <- use (ann . at (b ^. unId))
  _
