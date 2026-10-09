-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Linker where

import Control.Monad (foldM)
import Control.Monad.State (State, evalState, get, gets, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.QBE (Definition (DefData, DefFile, DefFunc, DefType), Program, localDefs, typeDefs)
import Language.QBE.Types qualified as QBE

data Env
  = Env
  { envGlobals :: Map QBE.GlobalIdent Int,
    envTypes :: Map QBE.UserIdent Int
  }

mkEnv :: Env
mkEnv = Env Map.empty Map.empty

------------------------------------------------------------------------

renameIdent :: Map QBE.GlobalIdent Int -> QBE.GlobalIdent -> QBE.GlobalIdent
renameIdent varOcc i = maybe i (incrGlobal i) $ Map.lookup i varOcc
  where
    incrGlobal :: QBE.GlobalIdent -> Int -> QBE.GlobalIdent
    incrGlobal global@(QBE.GlobalIdent s) n
      | n >= 1 = QBE.GlobalIdent $ s ++ "." ++ show n
      | otherwise = global

renameType :: Map QBE.UserIdent Int -> QBE.UserIdent -> QBE.UserIdent
renameType varOcc i = maybe i (incrGlobal i) $ Map.lookup i varOcc
  where
    incrGlobal :: QBE.UserIdent -> Int -> QBE.UserIdent
    incrGlobal global@(QBE.UserIdent s) n
      | n >= 1 = QBE.UserIdent $ s ++ "." ++ show n
      | otherwise = global

renameDef :: Env -> Definition -> Definition
renameDef Env {envGlobals = varOcc, envTypes = tyOcc} (DefFunc funcDef) =
  DefFunc $ QBE.mapGlobals (QBE.mapType funcDef (renameType tyOcc)) (renameIdent varOcc)
renameDef Env {envGlobals = varOcc} (DefData dataDef) =
  DefData $ QBE.mapGlobals dataDef (renameIdent varOcc)
renameDef Env {envTypes = tyOcc} (DefType typeDef) =
  DefType $ QBE.mapType typeDef (renameType tyOcc)
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

linkProgs :: [Program] -> State Env Program
linkProgs = foldM (\acc x -> (acc ++) <$> uniqProg x) []

link :: [Program] -> Program
link progs = evalState (linkProgs progs) mkEnv
