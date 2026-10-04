-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module provides a parser for the ar(5) file format. The
-- implementation provided here is modeled after OpenBSD's
-- [ar(5)](https://man.openbsd.org/ar.5) man page.
module Data.Archive (Object (..), Data.Archive.parse) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, void)
import Data.Maybe (catMaybes)
import Numeric (readOct)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    SourceName,
    anyChar,
    char,
    count,
    digit,
    eof,
    many,
    noneOf,
    octDigit,
    optionMaybe,
    parse,
    skipMany,
    string,
  )

data Object
  = Object
  { oName :: String,
    oTime :: Int,
    oUser :: Int,
    oGroup :: Int,
    oMode :: Int,
    oSize :: Int,
    oData :: String
  }
  deriving (Show, Eq)

magicString :: Parser ()
magicString = void $ string "!<arch>\n"

headerTrailer :: Parser ()
headerTrailer = void $ string "`\n"

lexme :: Parser a -> Parser a
lexme p = p <* skipMany (char ' ')

upTo :: Int -> Parser a -> Parser [a]
upTo n p
  | n <= 0 = return []
  | otherwise = catMaybes <$> replicateM n (optionMaybe p)

------------------------------------------------------------------------

-- From OpenBSD's ar(5):
--
-- > Only the name field has any provision for overflow. If any file name
-- > is more than 16 characters in length or contains an embedded space,
-- > the string "#1/" followed by the ASCII length of the name is written
-- > in the name field.
--
objName :: Parser (Either Int String)
objName =
  Left <$> (string "#1/" >> read <$> upTo 13 digit)
    <|> Right <$> upTo 16 (noneOf " ")

objTime :: Parser Int
objTime = read <$> upTo 12 digit

objUser :: Parser Int
objUser = read <$> upTo 6 digit

objGroup :: Parser Int
objGroup = objUser

objMode :: Parser Int
objMode = fst . head . readOct <$> upTo 8 octDigit

objSize :: Parser Int
objSize = read <$> upTo 10 digit

------------------------------------------------------------------------

-- From OpenBSD's ar(5):
--
-- > Objects in the archive are always an even number of bytes long; files
-- > which are an odd number of bytes long are padded with a newline (‘\n’)
-- > character, although the size in the header does not reflect this.
--
skipPadding :: Int -> Parser ()
skipPadding size
  | odd size = void (char '\n')
  | otherwise = pure ()

object :: Parser Object
object = do
  name <- lexme objName
  time <- lexme objTime
  oUid <- lexme objUser
  oGid <- lexme objGroup
  mode <- lexme objMode
  size <- lexme objSize

  headerTrailer
  (name', size') <-
    -- Overflow handling for name, see the 'objName' comment above.
    case name of
      Left ns ->
        if ns > size
          then fail "name length exceeds object length"
          else (,size - ns) <$> count ns anyChar
      Right n -> pure (n, size)

  body <- count size' anyChar <* skipPadding size'
  pure $
    Object
      { oName = name',
        oTime = time,
        oUser = oUid,
        oGroup = oGid,
        oMode = mode,
        oSize = size,
        oData = body
      }

parse :: SourceName -> String -> Either ParseError [Object]
parse =
  Text.ParserCombinators.Parsec.parse
    ((magicString >> many object) <* eof)
