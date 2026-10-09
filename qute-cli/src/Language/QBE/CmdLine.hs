-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.CmdLine
  ( BasicArgs (..),
    basicArgs,
    loadProg,
  )
where

import Data.Archive (qbeArchive)
import Language.QBE (Program, parseAndFind)
import Language.QBE.Linker (link)
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT

-- | t'BasicArgs' can be combined/extended with additional parsers using
-- the '<*>' applicative operator provided by "Options.Applicative".
data BasicArgs = BasicArgs
  { -- | Start address of the general-purpose memory.
    optMemStart :: MEM.Address,
    -- | Size of the memory in bytes.
    optMemSize :: MEM.Size,
    -- | Path to an .ar archive to preload.
    optQBEArchive :: Maybe FilePath,
    -- | Path to the QBE input file.
    optQBEFile :: FilePath
  }

-- | "Options.Applicative" parser for t'BasicArgs'.
basicArgs :: OPT.Parser BasicArgs
basicArgs =
  BasicArgs
    <$> OPT.option
      OPT.auto
      ( OPT.long "memory-start"
          <> OPT.short 'm'
          <> OPT.value 0x10000
      )
    <*> OPT.option
      OPT.auto
      ( OPT.long "memory-size"
          <> OPT.short 's'
          <> OPT.value (1024 * 1024) -- 1 MB RAM
          <> OPT.help "Size of the memory region"
      )
    <*> OPT.optional
      ( OPT.strOption
          ( OPT.long "preload"
              <> OPT.short 'p'
              <> OPT.metavar "FILE"
              <> OPT.help "Preload the given .ar archive consisting of QBE files"
          )
      )
    <*> OPT.argument OPT.str (OPT.metavar "FILE")

------------------------------------------------------------------------

-- | Name of the entry function.
entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "main"

-- | Parse a file and find the 'entryFunc'.
parseEntryFile :: FilePath -> IO (Program, QBE.FuncDef)
parseEntryFile filePath =
  readFile filePath >>= parseAndFind entryFunc

-- | Load the program specified in 'BasicArgs'. Returns the
-- loaded 'Program' and the entry function.
loadProg :: BasicArgs -> IO (Program, QBE.FuncDef)
loadProg opts = do
  (prog, func) <- parseEntryFile $ optQBEFile opts
  linked <-
    case optQBEArchive opts of
      Nothing -> pure prog
      Just ar -> (\xs -> link $ prog : xs) <$> qbeArchive ar
  pure (linked, func)
