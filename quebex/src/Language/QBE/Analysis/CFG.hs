-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Analysis.CFG
  ( -- * Control Flow Graph
    Label,
    CFG (cfgFunction, cfgLabelMap, cfgBlockMap, cfgSuccessors),
    build,
    basicBlockToLabel,
    labelToBasicBlock,
    lookupSuccessors,

    -- * Graph Representation
    cfgEdges,
    cfgGraph,

    -- * Dominator Analysis
    cfgDomGraph,
    cfgStartRoot,
    cfgReturnRoot,
    cfgHaltRoot,
  )
where

import Data.Graph (Graph, buildG)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (singleton)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Language.QBE.Analysis.Graph qualified as DG
import Language.QBE.Types qualified as QBE

type Label = IntMap.Key

-- TODO: Need to figure this mess wrt. data types: Should this point to a Block
-- or a BlockIdent or a custom Block data structure which includes both? Hmhm…
data CFG
  = CFG
  { cfgMaxBound :: Int,
    cfgFunction :: QBE.FuncDef,
    cfgLabelMap :: Map QBE.BlockIdent Label,
    cfgBlockMap :: IntMap QBE.BlockIdent,
    cfgSuccessors :: IntMap Successors -- TODO: Use a Set or List here?
  }
  deriving (Show)

cfgEdges :: CFG -> [DG.Edge]
cfgEdges cfg = foldl go [] $ IntMap.toList (cfgSuccessors cfg)
  where
    go :: [DG.Edge] -> (Label, Successors) -> [DG.Edge]
    go acc (p, c) = acc ++ map (p,) (successorsToBlockList' c)

basicBlockToLabel :: CFG -> QBE.BlockIdent -> Maybe Label
basicBlockToLabel CFG {cfgLabelMap = m} blkId = Map.lookup blkId m

labelToBasicBlock :: CFG -> Label -> Maybe QBE.BlockIdent
labelToBasicBlock CFG {cfgBlockMap = m} label = IntMap.lookup label m

lookupSuccessors :: CFG -> QBE.BlockIdent -> Maybe [QBE.BlockIdent]
lookupSuccessors cfg ident = do
  label <- basicBlockToLabel cfg ident
  successorsToBlockList cfg <$> IntMap.lookup label (cfgSuccessors cfg)

------------------------------------------------------------------------

-- A basic block can have one unconditional successor or two possible successors
-- in the case of a conditional jump. In QBE, there can never be more than two.
data Successors
  = SuccNone
  | SuccUncond Label
  | SuccCond Label Label
  deriving (Show, Eq)

successorsToBlockList' :: Successors -> [Label]
successorsToBlockList' SuccNone = []
successorsToBlockList' (SuccUncond label) = singleton label
successorsToBlockList' (SuccCond ifT ifF) = [ifT, ifF]

successorsToBlockList :: CFG -> Successors -> [QBE.BlockIdent]
successorsToBlockList cfg succs = map getBlock (successorsToBlockList' succs)
  where
    getBlock :: Label -> QBE.BlockIdent
    getBlock label = fromJust $ IntMap.lookup label (cfgBlockMap cfg)

successorsToIntSet :: Successors -> IntSet
successorsToIntSet = IntSet.fromList . successorsToBlockList'

------------------------------------------------------------------------

haltIdent :: (QBE.BlockIdent, Label)
haltIdent = (QBE.BlockIdent "=halt", 0)

returnIdent :: (QBE.BlockIdent, Label)
returnIdent = (QBE.BlockIdent "=return", 1)

-- Keep in sync with 'haltIdent' and 'returnIdent'.
identStart :: Label
identStart = 2

build' :: Map QBE.BlockIdent Label -> [QBE.Block] -> [(IntMap.Key, Successors)]
build' labelMap = foldl go [(snd haltIdent, SuccNone), (snd returnIdent, SuccNone)]
  where
    getId :: QBE.BlockIdent -> Label
    getId ident = fromJust $ Map.lookup ident labelMap

    go acc block@(QBE.Block {QBE.label = ident}) =
      let succs = case QBE.term block of
            QBE.Jump target -> SuccUncond (getId target)
            QBE.Jnz _ i1 i2 -> SuccCond (getId i1) (getId i2)
            QBE.Return _ -> SuccUncond $ snd returnIdent
            QBE.Halt -> SuccUncond $ snd haltIdent
       in (getId ident, succs) : acc

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
    blocks = QBE.fBlock func

    blkIdLabels :: [(QBE.BlockIdent, Label)]
    blkIdLabels = [haltIdent, returnIdent] ++ zip (map QBE.label blocks) [identStart ..]

cfgGraph :: CFG -> Graph
cfgGraph cfg = buildG (snd haltIdent, cfgMaxBound cfg) $ cfgEdges cfg

------------------------------------------------------------------------

cfgDomGraph :: CFG -> DG.Graph
cfgDomGraph cfg@(CFG {cfgLabelMap = labelMap}) =
  IntMap.fromList $ map (\l -> (l, succSet l)) (Map.elems labelMap)
  where
    succSet :: Label -> IntSet
    succSet l =
      successorsToIntSet
        (fromJust $ IntMap.lookup l (cfgSuccessors cfg))

cfgStartRoot :: CFG -> DG.Rooted
cfgStartRoot cfg = (identStart, cfgDomGraph cfg)

cfgReturnRoot :: CFG -> DG.Rooted
cfgReturnRoot cfg = (snd returnIdent, cfgDomGraph cfg)

cfgHaltRoot :: CFG -> DG.Rooted
cfgHaltRoot cfg = (snd returnIdent, cfgDomGraph cfg)
