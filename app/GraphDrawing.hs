{-# LANGUAGE NoImplicitPrelude #-}

module GraphDrawing where

import Control.Lens
import Data.GraphViz hiding (DotGraph)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Attributes.HTML (Table (..))
import Data.GraphViz.Attributes.HTML qualified as HTML
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.IntMap qualified as IM
import Data.Set qualified as S
import Egraph
import Lude

-- Stolen from hegg

toDot ::
  (Signature f) =>
  Maybe (ann -> Text) ->
  (f EId -> Text) ->
  Egraph f ann ->
  DotGraph Text
toDot showAnn showNode eg = digraph (Data.GraphViz.Types.Monadic.Str "egraph") do
  globallAttrs
  for_ (econcretize eg) \(Id i, ens, ann) -> do
    drawOutEdges eg i ens
    subgraph (Str $ "outercluster_" <> show i) $ do
      subgraph (Str $ "cluster_" <> show i) $ do
        -- add analysis text to e-class if anlText is available
        _ <- case showAnn of
          Nothing -> pass
          Just showAnn' -> graphAttrs [toLabel $ "class " <> show i <> ": " <> showAnn' ann]
        forM_ (zip ens [1 :: Integer ..]) $ \(n, j) -> do
          -- draw e-node (assign Dot node ID, assign label)
          -- Note: we throw out the final state but efind is not actually stateful,
          -- it just reads (not writes)
          let n' = evaluatingState eg (traverse efind n)
          node
            (show i <> "." <> show j)
            [ toLabel $
                HTML.Table $
                  hiddenTable (showNode n') (length n'),
              styles [rounded, filled],
              fillColor White,
              shape BoxShape
            ]
  where
    globallAttrs = do
      graphAttrs
        [ Compound True,
          ClusterRank Local,
          FontSize 9,
          NodeSep 0.05,
          RankSep [0.6],
          OutputOrder EdgesFirst,
          styles [dashed, rounded, filled]
        ]
      edgeAttrs
        [ ArrowSize 0.5
        ]
      nodeAttrs
        [ shape PlainText,
          FontName "helvetica"
        ]

sourceLabel :: Int -> Int -> Text
sourceLabel i j =
  show i <> "." <> show j

-- | For a given e-graph `eg`, draw all the out-going edges for nodes from a e-class with ID `classId`. Idealy we want the edges to point to e-classes as oppose to e-nodes, but because of how GraphViz edges are forces to point edge to a specific e-node. We can simulate the visual by "cliping" the arrows head to the e-class subgraph (using the LHead attribute). This specific e-node can chosen as the first node in the e-class, and there is no need to find the canonical node.
drawOutEdges :: Signature f => Egraph f ann -> Int -> [f EId] -> Dot Text
drawOutEdges eg i ns = for_ (zip ns [1 ..])
  $ \(n, i_in_class) -> do
    let n' = evaluatingState eg (traverse efind n)
    forM_ (zip (toList n') [(1 :: Integer) ..])
      $ \(child, arg_i) -> do
        let edgeSource = sourceLabel i i_in_class
        edge
          edgeSource
          (show (child ^. unId) <> ".1")
          [ LHead ("cluster_" <> show (child ^. unId)),
            TailPort $ LabelledPort (PN $ show arg_i) $ Just South
          ]

-- | Each e-node with text `t` of arity `n` is drawn as a table with n cols and 2 rows. The first row contain a single cell spanning n cols and displaying the  e-node text `t`. The second row contains "empty" cells having zero heights functioning as anchor point (used in each edge's TailPort attribute). This eliminates the need to use edge labels as a mean to understand positional arguments.
hiddenTable ::
  -- | text used to display the e-node
  Text ->
  -- | arity of the enode
  Int ->
  HTML.Table
hiddenTable t arity =
  HTML.HTable
    { tableFontAttrs = Nothing,
      tableAttrs = [HTML.CellSpacing 0, HTML.CellPadding 0, HTML.CellBorder 0, HTML.Align HTML.HCenter, HTML.Style HTML.Rounded, HTML.Border 0],
      tableRows = rs
    }
  where
    rs =
      [ HTML.Cells
          [ HTML.LabelCell
              [ HTML.ColSpan (fromIntegral $ max 1 arity),
                HTML.Width 30,
                HTML.Height 30,
                HTML.CellPadding 4
              ]
              $ HTML.Text [HTML.Str (toLText t)]
          ],
        HTML.Cells mkPortRow
      ]

    mkPortRow = (<$> [1 .. max 1 arity]) $ \i ->
      HTML.LabelCell
        [ HTML.Port
            $ PN
            $ show i
        ]
        $ HTML.Text []