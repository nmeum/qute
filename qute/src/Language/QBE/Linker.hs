-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module implements a "linker" for QBE source files. It takes multiple
-- 'Language.QBE.Program' values and merges them by resolving conflicting
-- non-exporting definitions by assising them a new (unique) name.
module Language.QBE.Linker (link) where

import Control.Monad (foldM)
import Control.Monad.State (State, evalState, get, gets, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.QBE (Definition (DefData, DefFile, DefFunc, DefType), Program, localDefs, typeDefs)
import Language.QBE.Types qualified as QBE
import Language.QBE.Uses qualified as USE

data Env
  = Env
  { envGlobals :: Map QBE.GlobalIdent Int,
    envTypes :: Map QBE.UserIdent Int
  }

mkEnv :: Env
mkEnv = Env Map.empty Map.empty

------------------------------------------------------------------------

-- TODO: Some sort of type class for Idents?
rename :: (Ord a) => (a -> Int -> a) -> Map a Int -> a -> a
rename inc varOcc i = maybe i (inc' i) $ Map.lookup i varOcc
  where
    inc' ident n = if n >= 1 then inc ident n else ident

renameGlobal :: Map QBE.GlobalIdent Int -> QBE.GlobalIdent -> QBE.GlobalIdent
renameGlobal = rename (\(QBE.GlobalIdent s) n -> QBE.GlobalIdent $ s ++ ".QG" ++ show n)

renameType :: Map QBE.UserIdent Int -> QBE.UserIdent -> QBE.UserIdent
renameType = rename (\(QBE.UserIdent s) n -> QBE.UserIdent $ s ++ ".QU" ++ show n)

renameDef :: Env -> Definition -> Definition
renameDef Env {envGlobals = varOcc, envTypes = tyOcc} (DefFunc funcDef) =
  DefFunc $ USE.mapGlobals (USE.mapType funcDef (renameType tyOcc)) (renameGlobal varOcc)
renameDef Env {envGlobals = varOcc} (DefData dataDef) =
  DefData $ USE.mapGlobals dataDef (renameGlobal varOcc)
renameDef Env {envTypes = tyOcc} (DefType typeDef) =
  DefType $ USE.mapType typeDef (renameType tyOcc)
renameDef _ def@(DefFile _) = def

renameDefs :: Env -> Program -> Program
renameDefs env = map (renameDef env)

renameProg :: Program -> State Env Program
renameProg prog = gets (`renameDefs` prog)

------------------------------------------------------------------------

count :: (Ord a) => Map a Int -> [a] -> Map a Int
count knownVars defs =
  let inc = maybe 0 (+ 1)
   in foldl (flip $ upsert inc) knownVars defs
  where
    -- TODO: Remove this once we upgrade to containers >= 0.8.1.
    -- See: <https://github.com/haskell/containers/issues/809>.
    upsert f = Map.alter (Just . f)

uniqProg :: Program -> State Env Program
uniqProg prog = do
  Env {envGlobals = knownVars, envTypes = knownTypes} <- get
  let varOcc = count knownVars (localDefs prog)
      tyOcc = count knownTypes (typeDefs prog)
  modify (\e -> e {envGlobals = varOcc, envTypes = tyOcc})
  renameProg prog

-- | Merge multiple QBE sources files into one, renaming non-exported definitions.
link :: [Program] -> Program
link progs = evalState (foldM go [] progs) mkEnv
  where
    go acc x = (acc ++) <$> uniqProg x
