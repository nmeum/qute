-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Default.Funcs (lookupSimFunc) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array (Array, bounds, listArray, (!))
import Data.Bits ((.&.))
import Data.ByteString.Lazy qualified as B
import Data.Word (Word32, Word64)
import Language.QBE.Simulator.ABI.Errno qualified as ERR
import Language.QBE.Simulator.ABI.POSIX qualified as POSIX
import Language.QBE.Simulator.Error (EvalError (FuncArgsMismatch))
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.State (Simulator, readArray, toAddress)
import Language.QBE.Types qualified as QBE
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (Handle, stderr, stdin, stdout)

mkErrno :: (E.ValueRepr v) => ERR.Errno -> v
mkErrno err =
  E.fromLit (QBE.Base QBE.Long) $
    -(fromIntegral $ ERR.errno err)

toByteString :: (E.ValueRepr v) => [v] -> B.ByteString
toByteString = B.pack . map (fromIntegral . E.toWord64)

readArray' :: (Simulator m v, E.ValueRepr v) => v -> QBE.LoadType -> v -> m [v]
readArray' startPtr elemTy size = do
  startAddr <- toAddress startPtr
  readArray startAddr elemTy $ E.toWord64 size

------------------------------------------------------------------------

data FileEntry
  = FileEntry
  { fdFlags :: Word32,
    -- TODO: Use a polymoprh representation for `fdHandle` which is
    -- parameterized over a `ValueRepr` to allow an implementation
    -- of custom file-like objects.
    fdHandle :: Handle
  }

isWritable :: FileEntry -> Bool
isWritable FileEntry {fdFlags = flags} =
  flags .&. POSIX.oWRONLY /= 0

writeEntry :: (MonadIO m) => (E.ValueRepr v) => FileEntry -> [v] -> m (Maybe Word64)
writeEntry entry bytes
  | isWritable entry = do
      let bs = toByteString bytes
      liftIO $ B.hPut (fdHandle entry) bs
      pure $ Just (fromIntegral $ B.length bs)
  | otherwise = pure Nothing

------------------------------------------------------------------------

-- TODO: Track open files in the Simulator Env to allow opening files.
openFiles :: Array Int FileEntry
openFiles =
  listArray
    (0, 2)
    [ FileEntry POSIX.oRDONLY stdin,
      FileEntry POSIX.oWRONLY stdout,
      FileEntry POSIX.oWRONLY stderr
    ]

getFile :: Int -> Maybe FileEntry
getFile fd =
  let (_, fdMax) = bounds openFiles
   in if fd > fdMax
        then Nothing
        else Just $ openFiles ! fd

------------------------------------------------------------------------

sysWrite :: (MonadIO m, E.ValueRepr v, Simulator m v) => QBE.GlobalIdent -> [v] -> m (Maybe v)
sysWrite _ [fd, arrayPtr, size] = do
  case getFile (fromIntegral $ E.toWord64 fd) of
    Nothing -> pure $ Just (mkErrno ERR.EBADF)
    Just fe -> do
      bArray <- readArray' arrayPtr (QBE.LSubWord QBE.SignedByte) size
      retVal <- writeEntry fe bArray
      pure $ case retVal of
        Nothing -> Just $ mkErrno ERR.EIO
        Just nb -> Just $ E.fromLit (QBE.Base QBE.Word) nb
sysWrite ident _ = throwError $ FuncArgsMismatch ident

sysExit :: (MonadIO m, E.ValueRepr v, Simulator m v) => QBE.GlobalIdent -> [v] -> m (Maybe v)
sysExit _ [status] =
  let code = fromIntegral $ E.toWord64 status
   in liftIO $ exitWith (ExitFailure code)
sysExit ident _ = throwError $ FuncArgsMismatch ident

------------------------------------------------------------------------

-- TODO: Register functions dynamically.
lookupSimFunc ::
  (MonadIO m, E.ValueRepr v, Simulator m v) =>
  QBE.GlobalIdent ->
  Maybe ([v] -> m (Maybe v))
lookupSimFunc i@(QBE.GlobalIdent "qute_exit") = Just (sysExit i)
lookupSimFunc i@(QBE.GlobalIdent "qute_write") = Just (sysWrite i)
lookupSimFunc _ = Nothing
