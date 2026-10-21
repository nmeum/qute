-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.ABI.Errno (Errno (..), errno) where

data Errno
  = EIO
  | EBADF

-- Keep this in sync with `include/scc/bits/qute/sys/errno.h`.
errno :: Errno -> Int
errno EIO = 5
errno EBADF = 9
