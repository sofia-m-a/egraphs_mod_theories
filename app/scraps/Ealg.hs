data W f a b
  = WC
  { _wcFork :: a -> a -> b,
    _wcNode :: f a -> a -> b
  }

data Eqrel
  = EqrelC
  { _eqrelRoot :: IntMap EId,
    _eqrelBack :: IntMap IntSet
  }

makeLenses ''Eqrel

efind :: EId -> Lens' Eqrel EId
efind e = eqrelRoot . at (e ^. unId) . anon e (== e)

eback :: EId -> Lens' Eqrel IntSet
eback e = eqrelBack . at (e ^. unId) . anon IntSet.empty IntSet.null

eqrelW :: EId -> EId -> State Eqrel (Maybe EId)
eqrelW a b = do
  a' <- use (efind a)
  b' <- use (efind b)
  if a == b
    then pure Nothing
    else do
      sa <- use (eback a')
      sb <- use (eback b')
      let (root, child) = if IntSet.size sa > IntSet.size sb then (a', b') else (b', a')
      efind child .= root
      old <- eback child <<.= IntSet.empty
      for_ (IntSet.toList old) (\o -> efind (Id o) .= root)
      eback root <>= one (child ^. unId) <> old
      pure (Just root)

data CC f
  = CCC
  { _ccEqrel :: Eqrel,
    _ccForward :: Map (f EId) EId,
    _ccBackward :: IntMap (Set (f EId))
  }

makeLenses ''CC

ccback :: EId -> Lens' (CC f) (Set (f EId))
ccback e = ccBackward . at (e ^. unId) . anon Set.empty Set.null

ccW :: (Signature f) => W f EId (State (CC f) (Maybe EId))
ccW = WC merge prop
  where
    merge a b =
      zoom ccEqrel (eqrelW a b)
        >>= maybe
          (pure Nothing)
          ( \r -> do
              let (old, new) = if r == a then (b, a) else (a, b)
              old <- ccback old <<.= Set.empty
              for_
                old
                ( \f -> do
                    f' <- traverse (\e -> use (ccEqrel . efind e)) f
                    c <- ccForward . at f <<.= Nothing
                    -- Shouldn't be Nothing...
                    whenJust c (void . prop f')
                )
              ccback new <>= old
              pure (Just r)
          )
    prop f a = do
      b <- ccForward . at f <<?= a
      for_ f (\x -> ccback x . contains f .= True)
      ccback a . contains f .= True
      case b of
        Just b' | b' /= a -> merge a b'
        _ -> pure Nothing

data AnnotatedCC f a
  = ACC
  { _accAnn :: IntMap a,
    _accCC :: CC f
  }

makeLenses ''AnnotatedCC

annW :: W f a (Maybe a) -> W f a (State (AnnotatedCC))
-- the point where I realized you need some kind of fixpoint over stuff
-- so that eg. ccW has to call the 'final' prop and merge, not its own