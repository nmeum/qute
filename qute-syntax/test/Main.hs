-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import Golden
import Parser
import System.Directory (findExecutable)
import System.IO (hPutStrLn, stderr)
import Test.Tasty
import Types

main :: IO ()
main = do
  hasQBE <- isJust <$> findExecutable "qbe"
  unless hasQBE $
    hPutStrLn stderr "WARNING: qbe(1) not installed, skipping Golden tests!"
  defaultMain $ tests hasQBE

-- Golden tests require qbe(1) to be installed, which is not available on Hackage.
tests :: Bool -> TestTree
tests hasQBE =
  let testTree =
        if hasQBE
          then baseTests ++ [goldenTests]
          else baseTests
   in testGroup "Tests" testTree
  where
    baseTests = [mkParser, typesTests]
