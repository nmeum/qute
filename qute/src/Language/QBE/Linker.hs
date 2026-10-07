-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Linker where

import Data.Map (Map)
import Data.Map qualified as Map
import Language.QBE (Definition (DefData, DefFunc), Program, localDefs)
import Language.QBE.Types qualified as QBE

data Env
  = Env
  { envGlobals :: Map QBE.GlobalIdent Int,
    envTypes :: Map QBE.GlobalIdent Int
  }

------------------------------------------------------------------------

renameIdent :: Map QBE.GlobalIdent Int -> QBE.GlobalIdent -> QBE.GlobalIdent
renameIdent varOcc i = maybe i (incrGlobal i) $ Map.lookup i varOcc
  where
    incrGlobal :: QBE.GlobalIdent -> Int -> QBE.GlobalIdent
    incrGlobal global@(QBE.GlobalIdent s) n
      | n >= 1 = QBE.GlobalIdent $ s ++ "." ++ show n
      | otherwise = global

renameDef :: Map QBE.GlobalIdent Int -> Definition -> Definition
renameDef varOcc (DefFunc funcDef) =
  DefFunc $ QBE.mapGlobals funcDef (renameIdent varOcc)
renameDef varOcc (DefData dataDef) =
  DefData $ QBE.mapGlobals dataDef (renameIdent varOcc)
renameDef _ def = def

renameGlobals :: Map QBE.GlobalIdent Int -> Program -> Program
renameGlobals varOcc = map (renameDef varOcc)

------------------------------------------------------------------------

cntLocals ::
  Map QBE.GlobalIdent Int ->
  Program ->
  Map QBE.GlobalIdent Int
cntLocals knownLocals prog =
  let locals = localDefs prog
   in foldl (flip $ upsert inc) knownLocals locals
  where
    inc = maybe 0 (+ 1)

    -- TODO: Remove this once we upgrade to containers >= 0.8.1.
    -- See: <https://github.com/haskell/containers/issues/809>.
    upsert f = Map.alter (Just . f)

uniqProg ::
  Map QBE.GlobalIdent Int ->
  Program ->
  (Map QBE.GlobalIdent Int, Program)
uniqProg knownLocals prog =
  let varOcc = cntLocals knownLocals prog
   in (varOcc, renameGlobals varOcc prog)

link :: [Program] -> Program
link = snd . foldl go (Map.empty, [])
  where
    go (k, l) p =
      let (nk, np) = uniqProg k p
       in (nk, l ++ np)
