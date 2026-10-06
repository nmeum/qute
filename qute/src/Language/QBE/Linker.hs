module Language.QBE.Linker where

import Language.QBE (Program, localDefs)

cntLocals ::
  Map QBE.GlobalIdent Int ->
  Program ->
  Map QBE.GlobalIdent Int
cntLocals knownLocals prog =
  let locals = localDefs prog
   in foldl (flip $ Map.upsert inc) knownLocals locals,
  where
    inc = maybe 1 (+1)

uniqIdents ::
  Map QBE.GlobalIdent Int ->
  [GlobalIdent] ->
  Map QBE.GlobalIdent QBE.GlobalIdent
uniqIdents known locals
  | Map.null known = Map.empty
  | otherwise = Map.fromList $ map go locals
  where
    go :: GlobalIdent -> Maybe (GlobalIdent, GlobalIdent)
    go prev@(GlobalIdent s) =
      case Map.lookup i known of
        Just n ->
          Just $ (prev, GlobalIdent $ s ++ "." ++ show n)
        Nothing -> Nothing

renameGlobals :: Map QBE.GlobalIdent Int -> Program -> Program
renameGlobals glbMap prog = _

uniqProg ::
  Map QBE.GlobalIdent Int ->
  Program ->
  (Map QBE.GlobalIdent Int, Program)
uniqProg knownLocals prog =
  let locals = localDefs prog
      glbMap = uniqIdents knownLocals locals
   in ( cntLocals knownLocals locals, renameGlobals glbMap prog )

link :: [Program] -> [Program]
link = foldl go []
  where
    go (k, l) p =
      let (nk, np) = uniqProg k p
       in (nk, np : l)

