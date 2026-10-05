-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Archive (archiveTests) where

import Control.Exception (throwIO)
import Data.Archive (Object (..), parse)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

parseFile :: FilePath -> IO [Object]
parseFile fileName = do
  content <- readFile $ "test" </> "testdata" </> "archive" </> fileName
  case parse fileName content of
    Right objs -> pure objs
    Left err -> throwIO err

------------------------------------------------------------------------

archiveTests :: TestTree
archiveTests =
  testGroup
    "Test the parser for the ar(5) format"
    [ testCase "single file" $
        do
          objs <- parseFile "single-file.ar"
          objs
            @?= [ Object
                    { oName = "hello.txt/",
                      oTime = 0,
                      oUser = 0,
                      oGroup = 0,
                      oMode = 0o644,
                      oSize = length "hello, world!\n",
                      oData = "hello, world!\n"
                    }
                ],
      testCase "single file with padding" $
        do
          objs <- parseFile "single-file-with-padding.ar"
          case objs of
            [obj] -> oData obj @?= "12\n" -- padding removed
            _ -> assertFailure "unexpected object amount",
      testCase "multiple files" $
        do
          objs <- parseFile "two-files-first-with-padding.ar"
          objs
            @?= [ Object
                    { oName = "world.txt/",
                      oTime = 0,
                      oUser = 0,
                      oGroup = 0,
                      oMode = 0o644,
                      oSize = length "world!\n",
                      oData = "world!\n"
                    },
                  Object
                    { oName = "hello.txt/",
                      oTime = 0,
                      oUser = 1000,
                      oGroup = 1005,
                      oMode = 0o644,
                      oSize = length "hello\n",
                      oData = "hello\n"
                    }
                ],
      testCase "file name with space" $
        do
          objs <- parseFile "file-name-containing-space.ar"
          case objs of
            -- See https://man.freebsd.org/cgi/man.cgi?query=ar&sektion=5
            [obj] -> do
              oName obj @?= "A B"
              oData obj @?= "C D"
            _ -> assertFailure "unexpected object amount"
    ]
