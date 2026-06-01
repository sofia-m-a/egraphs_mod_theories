{-# LANGUAGE TemplateHaskell #-}

module Signature where

import Control.Lens

newtype EId = Id {_unId :: Int}
  deriving (Eq, Ord, Hashable, Show, Read, Generic)
  deriving newtype (NFData)

makeWrapped ''EId

instance Bounded EId where
  minBound = Id 0
  maxBound = Id maxBound

makeLenses ''EId

class (Ord (ACSymbol enode), Ord (Symbol enode), Ord (enode EId), Traversable enode) => Signature enode where
  type Symbol enode

  symbolOf :: enode a -> Symbol enode
  default symbolOf :: (Symbol enode ~ enode ()) => enode a -> Symbol enode
  symbolOf = void

  type ACSymbol enode

  acSymbolOf :: enode a -> Maybe (ACSymbol enode)
  default acSymbolOf :: (ACSymbol enode ~ Void) => enode a -> Maybe (ACSymbol enode)
  acSymbolOf _ = Nothing

  -- arity :: enode a -> Int
  -- default arity :: (Symbol enode ~ enode ()) => enode a -> Int
  -- arity = length

  -- -- Who doesn't love ambiguity?
  -- arity' :: Proxy enode -> Symbol enode -> Int
  -- default arity' :: (Symbol enode ~ enode ()) => Proxy enode -> Symbol enode -> Int
  -- arity' _ = length

  -- arityAC :: Proxy enode -> ACSymbol enode -> Int
  -- default arityAC :: (ACSymbol enode ~ Void) => Proxy enode -> ACSymbol enode -> Int
  -- arityAC _ = absurd

  -- it has to be partial in certain cases...
  reconstruct :: Symbol enode -> [a] -> enode a
  default reconstruct :: (Symbol enode ~ enode ()) => Symbol enode -> [a] -> enode a
  reconstruct s as = s & unsafePartsOf traverse .~ as

  reconstructAC :: ACSymbol enode -> [a] -> enode a
  default reconstructAC :: (ACSymbol enode ~ Void) => ACSymbol enode -> [a] -> enode a
  reconstructAC = absurd

-- reconstruct :: Symbol enode -> [a] -> Maybe (enode a)
-- default reconstruct :: (Symbol enode ~ enode ()) => Symbol enode -> [a] -> Maybe (enode a)
-- reconstruct s as = sequence . (partsOf traverse .~ fmap Just as) . (Nothing <$) $ s

class (Signature f, NFData (f Int), NFData (ACSymbol f)) => NFSig f