-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Util where

import Control.Exception (throwIO)
import Data.Archive (Object, parse, qbeArchive)
import Language.QBE (Program)
import System.FilePath ((</>))

readArchive :: FilePath -> IO [Object]
readArchive fileName = do
  content <- readFile $ "test" </> "testdata" </> fileName
  case Data.Archive.parse fileName content of
    Right objs -> pure objs
    Left err -> throwIO err

parseArchive :: FilePath -> IO [Program]
parseArchive fileName =
  qbeArchive ("test" </> "testdata" </> fileName)
