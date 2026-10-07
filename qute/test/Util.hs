-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Util where

import Control.Exception (throwIO)
import Data.Archive (Object (Object, oData), parse)
import Language.QBE (Program, parse)
import System.FilePath ((</>))

readArchive :: FilePath -> IO [Object]
readArchive fileName = do
  content <- readFile $ "test" </> "testdata" </> "archive" </> fileName
  case Data.Archive.parse fileName content of
    Right objs -> pure objs
    Left err -> throwIO err

parseArchive :: FilePath -> IO [Program]
parseArchive fileName = do
  objs <- readArchive fileName
  mapM parseFile objs
  where
    parseFile :: Object -> IO Program
    parseFile (Object {oData = qbe}) =
      case Language.QBE.parse fileName qbe of
        Right prog -> pure prog
        Left err -> throwIO err
