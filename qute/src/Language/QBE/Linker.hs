-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Linker where

import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.QBE.Types qualified as QBE
import Language.QBE (Program, Definition(DefFunc), localDefs)

cntLocals ::
  Map QBE.GlobalIdent Int ->
  Program ->
  Map QBE.GlobalIdent Int
cntLocals knownLocals prog =
  let locals = localDefs prog
   in foldl (flip $ upsert inc) knownLocals locals
  where
    inc = maybe 1 (+1)

    -- TODO: Remove this once we upgrade to containers >= 0.8.1.
    -- See: <https://github.com/haskell/containers/issues/809>.
    upsert f = Map.alter (Just . f)

uniqIdents ::
  Map QBE.GlobalIdent Int ->
  [QBE.GlobalIdent] ->
  Map QBE.GlobalIdent QBE.GlobalIdent
uniqIdents known locals
  | Map.null known = Map.empty
  | otherwise = Map.fromList $ mapMaybe go locals
  where
    go :: QBE.GlobalIdent -> Maybe (QBE.GlobalIdent, QBE.GlobalIdent)
    go prev@(QBE.GlobalIdent s) =
      case Map.lookup prev known of
        Just n ->
          Just $ (prev, QBE.GlobalIdent $ s ++ "." ++ show n)
        Nothing -> Nothing

-- TODO: Code duplication.
isGlobal :: [QBE.Linkage] -> Bool
isGlobal = elem QBE.LExport

renameDef :: Map QBE.GlobalIdent QBE.GlobalIdent -> Definition -> Definition
renameDef glbMap (DefFunc funcDef)
  | isGlobal (QBE.fLinkage funcDef) = DefFunc funcDef
  | otherwise = DefFunc $ QBE.mapGlobals funcDef go
  where
    go :: QBE.GlobalIdent -> QBE.GlobalIdent
    go i =
      case Map.lookup i glbMap of
        Just r -> r
        Nothing -> i
renameDef _ def = def

renameGlobals :: Map QBE.GlobalIdent QBE.GlobalIdent -> Program -> Program
renameGlobals glbMap prog = map (renameDef glbMap) prog

uniqProg ::
  Map QBE.GlobalIdent Int ->
  Program ->
  (Map QBE.GlobalIdent Int, Program)
uniqProg knownLocals prog =
  let locals = localDefs prog
      glbMap = uniqIdents knownLocals locals
   in ( cntLocals knownLocals prog, renameGlobals glbMap prog )

link :: [Program] -> [Program]
link = snd . foldl go (Map.empty, [])
  where
    go (k, l) p =
      let (nk, np) = uniqProg k p
       in (nk, np : l)
