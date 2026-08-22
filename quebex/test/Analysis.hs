-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Analysis (analTests) where

import Data.Bifunctor (bimap)
import Data.List (sort)
import Language.QBE (parseAndFind)
import Language.QBE.Analysis.CDG qualified as CDG
import Language.QBE.Analysis.CFG qualified as CFG
import Language.QBE.Types qualified as QBE
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

getFunction :: QBE.GlobalIdent -> String -> IO QBE.FuncDef
getFunction funcName input = snd <$> parseAndFind funcName input

getFuncAndProg :: FilePath -> QBE.GlobalIdent -> IO QBE.FuncDef
getFuncAndProg fileName funcName =
  let filePath = "test" </> "testdata" </> fileName
   in readFile filePath >>= getFunction funcName

toBlkName :: CFG.CFG -> CFG.Label -> String
toBlkName cfg = show . CFG.labelToIdent cfg

cdgEdges :: CFG.CFG -> CDG.CDG -> [(String, String)]
cdgEdges cfg = map go . CDG.edges
  where
    go (f, t) = (toBlkName cfg f, toBlkName cfg t)

cfgEdges :: CFG.CFG -> [(String, String)]
cfgEdges cfg =
  let toBlk = toBlkName cfg
   in sort $ map (bimap toBlk toBlk) (CFG.edges cfg)

------------------------------------------------------------------------

analTests :: TestTree
analTests =
  testGroup
    "Analysis tests"
    [ testCase "Simple CFG without any loops" $
        do
          func <-
            getFunction
              (QBE.GlobalIdent "foo")
              "function w $foo() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \jmp @next\n\
              \@next\n\
              \ret\n\
              \}\n"

          let cfg = CFG.build func
          let startLabel = CFG.identToLabel cfg $ QBE.BlockIdent "start"
          map (CFG.labelToIdent cfg) (CFG.lookupSuccs cfg startLabel)
            @?= [QBE.BlockIdent "next"]

          cfgEdges cfg
            @?= [("@start", "@next")]

          -- “If Y is control dependent on X then X must have two exits.“, in
          -- this CFG there are no nodes with two exits: The CDG must be emtpy.
          let ret = CFG.identToLabel cfg $ QBE.BlockIdent "next"
              cdg = CDG.build cfg ret
          cdgEdges cfg cdg @?= []
          CDG.ctrlDeps cdg ret @?= Nothing,
      testCase "Generate CDG for code with single branch" $
        do
          func <-
            getFunction
              (QBE.GlobalIdent "foo")
              "function w $foo() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \jnz %val, @ifT, @ifF\n\
              \@ifT\n\
              \%ret =w copy 1\n\
              \jmp @return\n\
              \@ifF\n\
              \%ret =w copy 0\n\
              \jmp @return\n\
              \@return\n\
              \ret %ret\n\
              \}\n"

          let cfg = CFG.build func
              ret = CFG.identToLabel cfg (QBE.BlockIdent "return")
              cdg = CDG.build cfg ret

          cdgEdges cfg cdg
            @?= [ ("@ifF", "@start"),
                  ("@ifT", "@start")
                ],
      testCase "Compute CDG for code with loop" $
        do
          func <-
            getFunction
              (QBE.GlobalIdent "main")
              "function w $main() {\n\
              \@start\n\
              \%.1 =w copy 0\n\
              \%.2 =w copy 42\n\
              \%.3 =w copy 0\n\
              \@for_cond\n\
              \%.6 =w csltw %.3, %.2\n\
              \jnz %.6, @for_body, @for_join\n\
              \@for_body\n\
              \%.1 =w add %.1, 1\n\
              \@for_cont\n\
              \%.3 =w add %.3, 1\n\
              \jmp @for_cond\n\
              \@for_join\n\
              \ret %.11\n\
              \}\n"

          let cfg = CFG.build func
              ret = CFG.identToLabel cfg (QBE.BlockIdent "for_join")
              cdg = CDG.build cfg ret

          cdgEdges cfg cdg
            @?= [ ("@for_body", "@for_cond"),
                  ("@for_cond", "@for_cond"),
                  ("@for_cont", "@for_cond")
                ],
      testCase "Compute CDG for code with two paths to node" $
        do
          func <- getFuncAndProg "disjunction.qbe" (QBE.GlobalIdent "main")

          let cfg = CFG.build func
              ret = CFG.identToLabel cfg (QBE.BlockIdent "return")
              cdg = CDG.build cfg ret

          cdgEdges cfg cdg
            @?= [ ("@if_false.4", "@body.2"),
                  ("@if_false.4", "@if_true.3"),
                  ("@if_false.6", "@if_true.3"),
                  ("@if_join.7", "@if_true.3"),
                  ("@if_true.3", "@body.2"),
                  ("@if_true.5", "@if_true.3")
                ],
      testCase "Compute dominators for a simple-cc representation" $
        do
          func <- getFuncAndProg "simple-cc-branches.qbe" (QBE.GlobalIdent "myfunc")

          let cfg = CFG.build func
          CFG.labelToIdent cfg (CFG.startNode cfg)
            @?= QBE.BlockIdent ".L9"
    ]
