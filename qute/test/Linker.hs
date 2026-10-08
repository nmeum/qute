-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Linker (linkerTests) where

import Control.Exception (throwIO)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Language.QBE
  ( Definition (DefData),
    ExecError (EUnknownEntry),
    Program,
    globalFuncs,
    localDefs,
    parse,
    typeDefs,
  )
import Language.QBE.Linker (link)
import Language.QBE.Types qualified as QBE
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Util (parseArchive)

parseFile :: FilePath -> IO Program
parseFile fileName = do
  content <- readFile $ "test" </> "testdata" </> "linker" </> fileName
  case parse fileName content of
    Right prog -> pure prog
    Left err -> throwIO err

findData :: Program -> [QBE.DataDef]
findData = mapMaybe getData
  where
    getData :: Definition -> Maybe QBE.DataDef
    getData (DefData dataDef) = Just dataDef
    getData _ = Nothing

findFunc :: Program -> String -> IO QBE.FuncDef
findFunc prog name = do
  let funcs = globalFuncs prog
      entryIdent = QBE.GlobalIdent name
  case find (\f -> QBE.fName f == entryIdent) funcs of
    Just x -> pure x
    Nothing -> throwIO $ EUnknownEntry entryIdent

------------------------------------------------------------------------

linkerTests :: TestTree
linkerTests =
  testGroup
    "Test the QBE Linker"
    [ testCase "Link file with same data definition" $
        do
          prog <- parseFile "data-definition-and-call.qbe"
          let linked = link [prog, prog]

          map QBE.name (findData linked)
            @?= [QBE.GlobalIdent ".L116", QBE.GlobalIdent ".L116.1"]

          func <- findFunc (reverse linked) "__assert"
          let block = QBE.fEntry func
              funcVal = QBE.VConst (QBE.Const $ QBE.Global (QBE.GlobalIdent "error"))
              funcArg =
                QBE.ArgReg
                  (QBE.ABase QBE.Long)
                  (QBE.VConst (QBE.Const $ QBE.Global (QBE.GlobalIdent ".L116.1")))
          QBE.stmt block @?= [QBE.Call Nothing funcVal [funcArg]],
      testCase "Link files with helper function" $
        do
          progs <- parseArchive "linker/same-helper-function.ar"
          let linked = link progs

          localDefs linked
            @?= [QBE.GlobalIdent "myhelper", QBE.GlobalIdent "myhelper.1"]

          func <- findFunc linked "myfunc2"
          let block = QBE.fEntry func
              funcVal = QBE.VConst (QBE.Extern $ QBE.GlobalIdent "callback")
              funcArg =
                QBE.ArgReg
                  (QBE.ABase QBE.Long)
                  (QBE.VConst (QBE.Extern $ QBE.GlobalIdent "myhelper.1"))
          QBE.stmt block @?= [QBE.Call Nothing funcVal [funcArg]],
      testCase "Link files with conflicting type definition" $
        do
          progs <- parseArchive "linker/same-type-in-function-return.ar"
          let linked = link progs

          typeDefs linked
            @?= [QBE.UserIdent "retTy", QBE.UserIdent "retTy.1"]

          func <- findFunc linked "idivmax"
          QBE.fAbity func @?= Just (QBE.AUserDef $ QBE.UserIdent "retTy.1")
    ]
