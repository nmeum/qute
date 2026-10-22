-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module provides utility functions for interfacing with C code.
module Language.QBE.Simulator.ABI.C
  ( charRepr,
    readString,
    readArray,
    writeBuffer,
  )
where

import Control.Monad (foldM_)
import Data.Word (Word64)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State (Simulator, readMemory, writeMemory)
import Language.QBE.Types qualified as QBE

-- | Representation of a @char@ in C.
charRepr :: QBE.LoadType
charRepr = QBE.LSubWord QBE.SignedByte

-- | Read a null-terminated C string from memory at the given 'MEM.Address'.
-- The return value is a list of 8-bit values.
readString :: (Simulator m v) => MEM.Address -> m [v]
readString addr = go addr []
  where
    go a acc = do
      byte <- readMemory charRepr a
      if E.toWord64 byte == 0
        then pure acc
        else go (a + 1) (acc ++ [byte])
{-# INLINE readString #-}

-- | Read an array of a fixed size, starting from a given 'MEM.Address'.
-- The array is assumed to only store elements of the given 'QBE.LoadType'.
readArray :: (Simulator m v) => MEM.Address -> QBE.LoadType -> Word64 -> m [v]
readArray addr elemTy numElems =
  mapM (\off -> readMemory elemTy (addr + off)) [0 .. numElems - 1]
{-# INLINE readArray #-}

-- | Write a list of values to a memory buffer located at the given 'MEM.Address'.
writeBuffer :: (Simulator m v) => MEM.Address -> [v] -> m ()
writeBuffer = foldM_ go
  where
    go lastAddr el = do
      let elTy = E.getType el
          elSize = fromIntegral $ QBE.extTypeByteSize elTy
      writeMemory lastAddr elTy el
      pure $ lastAddr + elSize
{-# INLINE writeBuffer #-}
