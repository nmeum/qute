-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Analysis.CFG
  ( -- * Control Flow Graph
    Label,
    CFG (cfgFunction),
    build,
    identToLabel,
    labelToIdent,
    labelToBlock,
    lookupSuccs,

    -- * Graph Representation
    asGraph,
    nodes,
    edges,
    bounds,

    -- * Dominator Analysis
    asDomGraph,
    startNode,
  )
where

import Data.Graph (Bounds, Graph, buildG)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Language.QBE.Analysis.Graph qualified as DG
import Language.QBE.Types qualified as QBE

-- | Representation of a node in the 'CFG'.
type Label = IntMap.Key

-- | A representation of the control-flow within a 'QBE.FuncDef'.
data CFG
  = CFG
  { -- | Function for which this CFG was built.
    cfgFunction :: QBE.FuncDef,
    cfgMaxBound :: Int,
    cfgLabelMap :: Map QBE.BlockIdent Label,
    cfgBlockMap :: IntMap QBE.BlockIdent,
    cfgSuccessors :: IntMap [Label]
  }

-- | Returns a list of all graph nodes in an unspecified order.
nodes :: CFG -> [Label]
nodes = IntMap.keys . cfgBlockMap

-- | Returns a list of graph edges in an unspecified order.
edges :: CFG -> [(Label, Label)]
edges cfg = foldl go [] $ IntMap.toList (cfgSuccessors cfg)
  where
    go acc (p, c) = acc ++ map (p,) c

-- | Returns the bounds of the 'CFG'. This is useful, for example, to
-- build a subgraph using 'Data.Graph.buildG'.
bounds :: CFG -> Bounds
bounds cfg = (0, cfgMaxBound cfg)

-- | Convert a 'QBE.BlockIdent' to a CFG node 'Label'.
--
-- This function is partial, on an invalid 'Label', an error is thrown.
identToLabel :: CFG -> QBE.BlockIdent -> Label
identToLabel CFG {cfgLabelMap = m} blkId =
  fromJust $ Map.lookup blkId m

-- | Convert a CFG node 'Label' to a 'QBE.BlockIdent'.
--
-- This function is partial, on an invalid 'Label', an error is thrown.
labelToIdent :: CFG -> Label -> QBE.BlockIdent
labelToIdent CFG {cfgBlockMap = m} label =
  fromJust $ IntMap.lookup label m

-- | Utility function to convert a node 'Label' to a 'QBE.Block'.
-- Performs two \(O(\log n)\) lookups internally.
--
-- This function is partial, on an invalid 'Label', an error is thrown.
labelToBlock :: CFG -> Label -> QBE.Block
labelToBlock cfg label =
  let blocks = QBE.fBlock $ cfgFunction cfg
   in fromJust $ Map.lookup (labelToIdent cfg label) blocks

-- | Mapping of 'Label' to its successors in the CFG, represented as an
-- ordered list of zero, one, or two elements. A list with two elements
-- represents a conditional jump where the left child is the is the true
-- branch and the right child is the false branch. A list wih a single
-- element signifies an unconditional jump. If the given node does not
-- have any successors an empty list is returned.
--
-- This function is partial, on an invalid 'Label', an error is thrown.
lookupSuccs :: CFG -> Label -> [Label]
lookupSuccs CFG {cfgSuccessors = succs} label =
  fromJust $ IntMap.lookup label succs

------------------------------------------------------------------------

identStart :: Label
identStart = 0

-- | Construct a 'CFG' for a given function.
build :: QBE.FuncDef -> CFG
build func =
  CFG
    { cfgMaxBound = snd $ last blkIdLabels,
      cfgFunction = func,
      cfgLabelMap = labelMap,
      cfgBlockMap = IntMap.fromList $ map swap blkIdLabels,
      cfgSuccessors = IntMap.fromList $ build' labelMap blocks
    }
  where
    labelMap :: Map QBE.BlockIdent Label
    labelMap = Map.fromList blkIdLabels

    blocks :: [QBE.Block]
    blocks = Map.elems $ QBE.fBlock func

    blkIdLabels :: [(QBE.BlockIdent, Label)]
    blkIdLabels = zip (map QBE.label blocks) [identStart ..]

build' :: Map QBE.BlockIdent Label -> [QBE.Block] -> [(IntMap.Key, [Label])]
build' labelMap = foldl go []
  where
    toLabel :: QBE.BlockIdent -> Label
    toLabel ident = fromJust $ Map.lookup ident labelMap

    go acc block@(QBE.Block {QBE.label = ident}) =
      let succs = case QBE.term block of
            QBE.Jump target -> [toLabel target]
            QBE.Jnz _ i1 i2 -> [toLabel i1, toLabel i2]
            QBE.Return _ -> []
            QBE.Halt -> []
       in (toLabel ident, succs) : acc

------------------------------------------------------------------------

asGraph :: CFG -> Graph
asGraph cfg = buildG (identStart, cfgMaxBound cfg) $ edges cfg

asDomGraph :: CFG -> DG.Graph
asDomGraph cfg = IntMap.map IntSet.fromList (cfgSuccessors cfg)

-- | Determine the entry node of the 'CFG'. Useful, for example, to
-- generated a 'DG.Rooted' representation for the control-flow graph.
startNode :: CFG -> Label
startNode cfg@(CFG {cfgFunction = func}) =
  identToLabel cfg (QBE.fStart func)
