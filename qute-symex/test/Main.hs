-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import BV (bvTests)
import Backend (backendTests)
import Concolic qualified as CE
import Control.Exception (IOException, try)
import Data.Either (isLeft)
import Explorer (exploreTests)
import Golden (goldenTests)
import Language.QBE.Simulator.Explorer (defSolver)
import SimpleBV qualified as SMT
import Symbolic qualified as SE
import System.IO (hPutStrLn, stderr)
import Test.Tasty

main :: IO ()
main = do
  solver <- try defSolver :: IO (Either IOException SMT.Solver)
  if isLeft solver
    then hPutStrLn stderr "WARNING: No solver found, skipping tests!"
    else defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ SE.exprTests,
      CE.exprTests,
      backendTests,
      exploreTests,
      goldenTests,
      bvTests
    ]
