-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.ABI.POSIX where

import Data.Word (Word32)

oRDONLY :: Word32
oRDONLY = 0x00000000

oWRONLY :: Word32
oWRONLY = 0x00000001

oRDWR :: Word32
oRDWR = 0x00000002

------------------------------------------------------------------------

oACCMODE :: Word32
oACCMODE = 0x00000003

oCLOEXEC :: Word32
oCLOEXEC = 0x00400000

oEXCL :: Word32
oEXCL = 0x00000800

oTRUNC :: Word32
oTRUNC = 0x00000400

oCREAT :: Word32
oCREAT = 0x00000200

oAPPEND :: Word32
oAPPEND = 0x00000008
