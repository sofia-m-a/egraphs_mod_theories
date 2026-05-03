data AlgTest a
  = Add a a
  | Mul a a
  | Neg a
  | Unit
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Signature AlgTest where
  type Symbol AlgTest = AlgTest ()

data Rewrite enode var = RewriteC (F enode var) (F enode var)
  deriving (Functor, Foldable, Traversable)

data WeakContext symbol var
  = TopLevelVar var
  | InImmediateContext [(var, (symbol, Int))]

contextualize :: (Signature enode) => F enode var -> WeakContext (Symbol enode) var
contextualize = cata \case
  Pure a -> TopLevelVar a
  Free term ->
    InImmediateContext
      ( itoListOf folded term >>= \(i, ctx) -> case ctx of
          TopLevelVar var -> pure (var, (symbolOf term, i))
          InImmediateContext cs -> cs
      )

va, vb, vc :: Int
va = 1
vb = 2
vc = 3

t :: F AlgTest Int
t = wrap (Add (pure va) (wrap (Add (pure vb) (pure vc))))

data BadRule symbol var
  = CollapseRule var var

weakTermGraph :: (Signature enode) => [Rewrite enode var] -> (Graph, [(Vertex, Vertex)])
weakTermGraph rws = undefined -- (graph, _)
  where
    -- (graph, o, i) = graphFromEdges (_ $ concatMap analyze rws)
    analyze (RewriteC lhs rhs) = case (contextualize lhs, contextualize rhs) of
      -- x → x
      (TopLevelVar v1, TopLevelVar v2) | v1 == v2 -> Right []
      -- x → y
      (TopLevelVar v1, TopLevelVar v2) | otherwise -> Left (CollapseRule v1 v2)
      -- x → t
      (TopLevelVar v1, InImmediateContext c2) -> undefined
      -- t → x
      (InImmediateContext c1, TopLevelVar v2) -> undefined
      -- s → t
      (InImmediateContext c1, InImmediateContext c2) -> undefined