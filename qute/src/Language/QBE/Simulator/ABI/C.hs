-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module provides utility functions for interfacing with C code.
module Language.QBE.Simulator.ABI.C (readNullArray, readArray) where

import Data.Word (Word64)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State (Simulator, readMemory)
import Language.QBE.Types qualified as QBE

-- | Read a null-terminated C string from memory at the given 'MEM.Address'.
-- The return value is a list of 8-bit values.
readNullArray :: (Simulator m v) => MEM.Address -> m [v]
readNullArray addr = go addr []
  where
    go a acc = do
      byte <- readMemory (QBE.LSubWord QBE.SignedByte) a
      if E.toWord64 byte == 0
        then pure acc
        else go (a + 1) (acc ++ [byte])
{-# INLINE readNullArray #-}

-- | Read an array of a fixed size, starting from a given 'MEM.Address'.
-- The array is assumed to only store elements of the given 'QBE.LoadType'.
readArray :: (Simulator m v) => MEM.Address -> QBE.LoadType -> Word64 -> m [v]
readArray addr elemTy numElems =
  mapM (\off -> readMemory elemTy (addr + off)) [0 .. numElems - 1]
{-# INLINE readArray #-}
