{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use lambda-case" -}

module Egraph
  ( EId (..),
    unId,
    Signature (..),
    Egraph,
    efind,
    epropagate,
  )
where

import Control.Lens hiding (para)
import Control.Monad.Free (Free (..), iter)
import Data.Fix (Fix (Fix))
import Data.Functor.Foldable (cata)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lude
import Prettyprinter (Doc, indent, line, list, parens, viaShow, vsep, (<+>))
import Prettyprinter qualified as Pretty

newtype EId = Id {_unId :: Int}
  deriving (Eq, Ord, Hashable, Show, Read, Generic)

makeWrapped ''EId

instance Bounded EId where
  minBound = Id 0
  maxBound = Id maxBound

makeLenses ''EId

class (Ord (Symbol enode), Ord (enode EId), Traversable enode) => Signature enode where
  type Symbol enode

  symbolOf :: enode a -> Symbol enode
  default symbolOf :: (Symbol enode ~ enode ()) => enode a -> Symbol enode
  symbolOf = void

  reconstruct :: Symbol enode -> [a] -> Maybe (enode a)
  default reconstruct :: (Symbol enode ~ enode ()) => Symbol enode -> [a] -> Maybe (enode a)
  reconstruct s as = sequence . (partsOf traverse .~ fmap Just as) . (Nothing <$) $ s

-- class (Signature enode) => Analysis ann enode | ann -> enode where
--   annF :: enode ann -> ann
--   annMerge :: ann -> ann -> ann

-- -- trivial E-analysis
-- instance (Signature enode) => Analysis (Proxy enode) enode where
--   annF _ = Proxy
--   annMerge _ _ = Proxy

-- Maybe this could use IntMaps via some type family stuff. TODO
data IndexedPairMap k
  = IndexedPairMapC
  { _ipmForward :: Map (k, k) k,
    _ipmBackward :: Map k (Set (k, k)),
    _ipm1 :: Map k (Set k),
    _ipm2 :: Map k (Set k)
  }

makeLenses ''IndexedPairMap

emptyIPM :: IndexedPairMap k
emptyIPM = IndexedPairMapC Map.empty Map.empty Map.empty Map.empty

ipmR :: (Ord k) => k -> Getter (IndexedPairMap k) (Set (k, k))
ipmR k = ipmBackward . at k . anon Set.empty Set.null

ipmU1 :: (Ord k) => k -> Getter (IndexedPairMap k) (Set k)
ipmU1 k = ipm1 . at k . anon Set.empty Set.null

ipmU2 :: (Ord k) => k -> Getter (IndexedPairMap k) (Set k)
ipmU2 k = ipm2 . at k . anon Set.empty Set.null

ipmUses :: (Ord k) => k -> IndexedPairMap k -> [(k, k)]
ipmUses k ipm =
  (toList (ipm ^. ipmU1 k) <&> (k,))
    <> (toList (ipm ^. ipmU2 k) <&> (,k))

type instance Index (IndexedPairMap k) = (k, k)

type instance IxValue (IndexedPairMap k) = k

instance (Ord k) => Ixed (IndexedPairMap k)

instance (Ord k) => At (IndexedPairMap k) where
  at k = lens getIt setIt
    where
      getIt ipm = ipm ^. ipmForward . at k
      setIt ipm v =
        let (new, old) = (v, ipm ^. ipmForward . at k)
         in ipm
              & ipmForward . at k .~ new
              -- These lines need to be this way around (there was a bug before)
              -- It's a shame that this is not entirely obvious...
              -- Really, we would ideally like to express this algebraically
              -- via the semilattice (||) structure
              & maybe id (\o -> updateIndex k o False) old
              & maybe id (\n -> updateIndex k n True) new
      updateIndex (a, b) c v ipm =
        ipm
          & ipmBackward . at c . anon Set.empty Set.null . contains k .~ v
          & ipm1 . at a . anon Set.empty Set.null . contains b .~ v
          & ipm2 . at b . anon Set.empty Set.null . contains a .~ v

data Egraph enode
  = EgraphC
  { _egAtom :: IntMap EId,
    _egAtomT :: IntMap (Seq EId),
    _egPair :: IndexedPairMap EId,
    _egSym :: Map (Symbol enode) EId,
    _egSymR :: IntMap (Symbol enode),
    _egNext :: EId
  }

makeLenses ''Egraph

edebug :: (Symbol e -> Doc ann) -> Egraph e -> Doc ann
edebug showNode eg =
  "Egraph with"
    <+> viaShow (eg ^. egNext)
    <+> "ids"
      <> line
      <> indent
        2
        ( vsep
            [ "egAtom"
                <+> indent
                  2
                  ( vsep
                      $ toList
                      $ imap (\i (Id j) -> viaShow i <+> "→" <+> viaShow j) (eg ^. egAtom)
                  ),
              "egAtomT"
                <+> indent
                  2
                  ( vsep
                      $ toList
                      $ imap (\i ids -> viaShow i <+> "contains" <+> Pretty.list (viaShow . view unId <$> toList ids)) (eg ^. egAtomT)
                  ),
              "egPair"
                <+> indent
                  2
                  ( indent
                      2
                      ( vsep
                          [ "ipmForward"
                              <+> indent
                                2
                                ( vsep
                                    $ toList
                                    $ imap
                                      ( \(a, b) c ->
                                          showId a <> "," <+> showId b <+> "→" <+> showId c
                                      )
                                      (eg ^. egPair . ipmForward)
                                ),
                            "ipmBackward"
                              <+> indent
                                2
                                ( vsep
                                    $ toList
                                    $ imap
                                      ( \c abss ->
                                          showId c <+> "←" <+> list ((\(a, b) -> parens (showId a <> "," <+> showId b)) <$> toList abss)
                                      )
                                      (eg ^. egPair . ipmBackward)
                                ),
                            "ipm1"
                              <+> indent
                                2
                                ( vsep
                                    $ toList
                                    $ imap
                                      ( \a bs ->
                                          showId a <+> "left-pairs with" <+> list (showId <$> toList bs)
                                      )
                                      (eg ^. egPair . ipm1)
                                ),
                            "ipm2"
                              <+> indent
                                2
                                ( vsep
                                    $ toList
                                    $ imap
                                      ( \a bs ->
                                          showId a <+> "right-pairs with" <+> list (showId <$> toList bs)
                                      )
                                      (eg ^. egPair . ipm2)
                                )
                          ]
                      )
                  ),
              "egSym"
                <+> indent
                  2
                  ( vsep
                      $ toList
                      $ imap (\s i -> showNode s <+> "→" <+> showId i) (eg ^. egSym)
                  )
            ]
        )
  where
    showId i = viaShow (i ^. unId)

efind :: EId -> Lens' (Egraph enode) EId
efind e = egAtom . at (e ^. unId) . anon e (== e)

eatomT :: EId -> Lens' (Egraph enode) (Seq EId)
eatomT e = egAtomT . at (e ^. unId) . anon [] null

-- surely this has to be in lens somewhere?
neIso :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
neIso =
  iso
    ( \case
        [] -> Nothing
        (a : as) -> Just (a :| as)
    )
    ( \case
        Nothing -> []
        Just (a :| as) -> a : as
    )

workListFix :: (Traversable t, Monad m) => (a -> m (t a)) -> a -> m ()
workListFix f a = f a >>= traverse_ (workListFix f)

-- Some trickiness: we can't have a 'closed loop' here
-- because we really want to call prop/union on externally-annotated things.
eunion :: EId -> EId -> State (Egraph enode) EId
eunion a b = do
  workListFix epropagate (a, b)
  use (efind a)

eunion_ :: EId -> EId -> State (Egraph enode) ()
eunion_ a b = void (eunion a b)

epropagate :: (EId, EId) -> State (Egraph enode) [(EId, EId)]
epropagate (a', b') = do
  a <- use (efind a')
  b <- use (efind b')
  if a == b
    then pure []
    else do
      sa' <- use (eatomT a . to length)
      sb' <- use (eatomT b . to length)
      let (newRoot, newChild) = if sa' > sb' then (a, b) else (b, a)
      efind newChild .= newRoot
      old <- eatomT newChild <<.= []
      eatomT newRoot <>= one newChild <> old
      -- repoint all to new root
      for_ old (\e -> efind e .= newRoot)
      -- if there is a rule (a, b) -> newChild, turn it into (a, b) -> newRoot
      use (egPair . ipmR newChild)
        >>= traverse_ (\(a, b) -> egPair . at (a, b) ?= newRoot)
      -- if we have any rules either of the form (newChild, b) -> c or (a, newChild) -> c,
      -- rewrite them
      es' <- use egPair >>= traverse propPair . ipmUses newChild
      pure (es' >>= toList)
  where
    propPair (a, b) = do
      a' <- use (efind a)
      b' <- use (efind b)
      c <- egPair . at (a, b) <<.= Nothing
      clash <- egPair . at (a', b') <<.= c
      -- if we tried to update (a, b) -> c to (a', b') -> c, but already (a', b') -> d,
      -- then queue the merge (c, d)
      case (c, clash) of
        (Just c, Just d) | c /= d -> pure (Just (c, d))
        _ -> pure Nothing

enext :: State (Egraph enode) EId
enext = egNext <<%= (unId %~ (+ 1))

esym :: (Signature enode) => Symbol enode -> State (Egraph enode) EId
esym s =
  use (egSym . at s)
    >>= maybe
      ( do
          n <- enext
          egSym . at s ?= n
          pure n
      )
      pure

epair :: (EId, EId) -> State (Egraph enode) EId
epair (a, b) =
  use (egPair . at (a, b))
    >>= maybe
      ( do
          n <- enext
          egPair . at (a, b) ?= n
          pure n
      )
      pure

einsert :: (Signature enode) => enode EId -> State (Egraph enode) EId
einsert n =
  foldl'
    (\s b -> s >>= epair . (,b))
    (esym (symbolOf n))
    (toList n)

einsertFix :: (Signature enode) => Fix enode -> State (Egraph enode) EId
einsertFix = cata (sequence >=> einsert)

einsertFree :: (Signature f) => Free f EId -> State (Egraph f) EId
einsertFree = iter (sequence >=> einsert) . fmap pure

eempty :: Egraph enode
eempty = EgraphC mempty mempty emptyIPM Map.empty mempty (Id 0)

data Ex1 a
  = F a a
  | G a
  | H Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Signature Ex1 where
  type Symbol Ex1 = Ex1 ()

example1 :: Egraph Ex1
example1 = executingState eempty do
  a <- einsert (H 3)
  b <- einsert (H 4)
  c <- einsertFix $ Fix (F (Fix $ H 3) (Fix $ G (Fix $ H 4)))
  d <- einsertFix $ Fix (F (Fix $ H 4) (Fix $ H 5))
  eunion_ a b
  e <- einsert (H 5)
  f <- einsertFree (Free $ G (Pure e))
  eunion_ e f
  pass