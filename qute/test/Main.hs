-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Analysis
import Archive
import Expression
import Linker
import Memory
import Simulator
import State
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ archiveTests,
      linkerTests,
      simTests,
      memTests,
      analTests,
      exprTests,
      stateTests
    ]
