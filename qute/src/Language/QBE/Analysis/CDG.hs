-- SPDX-FileCopyrightText: 2010 Tristan Ravitch <travitch@cs.wisc.edu>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-only

-- Based on the implementation provided by LLVM.Analysis.CDG from Tristan Ravitch
-- See https://hackage.haskell.org/package/llvm-analysis-0.3.0/docs/src/LLVM-Analysis-CDG.html
--
-- The implementation by Tristan Ravitch mentions a paper by Cytron et al.
-- See: https://doi.org/10.1145/115372.115320
--
-- However, I found that the original paper by Ferrante et al. does a much better job at
-- explaining what was implemented by Tristan Ravitch in llvm-analysis. Hence, the comments
-- below mainly refer to that: https://doi.org/10.1145/24039.24041

-- | This module implements a control dependency analysis, using a
-- /control dependency graph/ (CDG) for more information on the concept
-- refer to <https://doi.org/10.1145/24039.24041>. Roughly speaking, a
-- node /A/ is control dependent on /B/ if there is an edge /B → A/ so
-- that the node is taken, as well as an edge so that it is not taken.
module Language.QBE.Analysis.CDG
  ( CDG (..),
    build,
    edges,
    ctrlDeps,
  )
where

import Data.Bifunctor (second)
import Data.IntMap (IntMap)
import Data.IntMap qualified as M
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.List (find)
import Data.Maybe (fromMaybe)
import Language.QBE.Analysis.CFG qualified as CFG
import Language.QBE.Analysis.Graph qualified as G

-- | A CDG signifying control-dependence between nodes in the 'CFG.CFG'.
data CDG
  = CDG
  { -- | Underlying 'CFG.CFG' for which the CDG was built.
    cdgCfg :: CFG.CFG,
    -- | Root node of the t'CDG', used for determining post-dominance.
    cdgRoot :: CFG.Label,
    -- | Graph representation of control-dependence.
    cdgGraph :: G.Graph
  }

-- | All edges of the t'CDG', in an unspecified order.
edges :: CDG -> [(CFG.Label, CFG.Label)]
edges cdg = foldl go [] $ M.toList (cdgGraph cdg)
  where
    go acc (p, c) = acc ++ map (p,) (S.toList c)

-- | Returns the control dependencies of a given node in the 'CFG.CFG'.
-- If the node doesn't have any control dependencies, 'Nothing' is
-- returned.
ctrlDeps :: CDG -> CFG.Label -> Maybe IntSet
ctrlDeps CDG {cdgGraph = cDeps} = (`M.lookup` cDeps)

------------------------------------------------------------------------

-- | Construct a new t'CDG' from an existing 'CFG.CFG'. The CDG is build
-- based on the given 'CFG.Label' from the CFG, which is used to as the
-- root of a post-dominator tree to establish a post-dominance
-- relationship between nodes.
build :: CFG.CFG -> CFG.Label -> CDG
build cfg root =
  CDG
    { cdgCfg = cfg,
      cdgRoot = root,
      cdgGraph = build' cfg root
    }

build' :: CFG.CFG -> CFG.Label -> IntMap IntSet
build' cfg label =
  -- From the CFG, generate a post-dominator tree and also convert this tree
  -- to an IntMap representation for efficient successor lookup in 'addCDGEdge'.
  let rooted = (label, CFG.asDomGraph cfg)
      pdTree = G.pdomTree rooted
      pdtMap = M.fromList $ map (second S.fromList) (G.pdom rooted)
      pdtAnc = M.fromList (G.ancestors pdTree)
   in foldr (uncurry $ addCDGEdge pdtMap pdtAnc) M.empty $ CFG.edges cfg

-- This function essentially implements the algorithm described in Section 3.1
-- of the Paper by Ferrante et al., using the algorithm by Cytron et al. may be
-- more efficient and could be considered in the future.
addCDGEdge ::
  IntMap IntSet ->
  IntMap [Int] ->
  CFG.Label ->
  CFG.Label ->
  IntMap IntSet ->
  IntMap IntSet
addCDGEdge pdtMap pdtAnc a b acc
  -- Consider all edges (A, B) in the control flow graph such that B does not
  -- post-dominate M. If it does, we return 'acc' unmodified (insert nothing).
  | postdominates b a = acc
  | otherwise =
      -- Let AC denote the least common ancestor of A and B in the post-dominator tree.
      case commonAncestor b a of
        -- Case 1: All nodes in the post-dominator tree on the path from AC to
        -- B, including B but not AC, should be made control dependent on A.
        Just ac ->
          let cdepsOnA = S.insert b (S.filter (/= ac) $ lookupSucc b)
           in foldr insertEdge acc (S.toList cdepsOnA)
        -- Case 2: All nodes in the post-dominator tree on the path from A to B,
        -- including A and B, should be made control dependent on A.
        Nothing ->
          let deps = S.insert b $ lookupSucc b
           in foldr insertEdge acc (S.toList deps)
  where
    insertEdge :: CFG.Label -> IntMap IntSet -> IntMap IntSet
    insertEdge blk = M.insertWith S.union blk (S.singleton a)

    lookupSucc :: CFG.Label -> IntSet
    lookupSucc l = fromMaybe S.empty $ M.lookup l pdtMap

    -- Returns true if 'x' post-dominates 'y'.
    postdominates :: CFG.Label -> CFG.Label -> Bool
    postdominates x y = maybe False (x `S.member`) $ M.lookup y pdtMap

    commonAncestor :: G.Node -> G.Node -> Maybe G.Node
    commonAncestor n1 n2 = do
      a1 <- M.lookup n1 pdtAnc
      a2 <- M.lookup n2 pdtAnc
      find (`elem` a1) a2
