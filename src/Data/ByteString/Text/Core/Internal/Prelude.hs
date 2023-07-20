{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
  #-}

{-| This module exists to avoid name clashes created by importing 'Prelude' unqualified.
-}
module Data.ByteString.Text.Core.Internal.Prelude (
($!),
HasCallStack, assert,
module GHC.Enum,
module GHC.Err,
module GHC.Exts,
#if !(__GLASGOW_HASKELL >= 902)
Word8#, wordToWord8#, word8ToWord#,
#endif
module GHC.Num,
module GHC.Real,
module Control.Applicative,
module Data.Bool,
module Data.Char, chr,
module Data.Eq,
module Data.Function,
module Data.Functor,
module Data.Int,
module Data.Ord,
module Data.List.NonEmpty,
module Data.Maybe,
module Data.Monoid,
module Data.Semigroup,
module Data.Tuple,
module Data.Word,
module Text.Read,
module Text.Show,
) where

import GHC.Base (($!))
import GHC.Enum
import GHC.Err (error, errorWithoutStackTrace, undefined)
-- Bring basic unlifted operators into scope when MagicHash is enabled:
import GHC.Exts
  ( Char#
  , eqChar#, neChar#, leChar#, ltChar#, geChar#, gtChar#
  , chr#
  , Int#
  , negateInt#, ( *# ), ( +# ), ( -# )
  , ( ==# ), ( /=# ), ( <=# ), ( <# ), ( >=# ), ( ># )
  , notI#, andI#, orI#, xorI#
  , Word#
  , eqWord#, neWord#, leWord#, ltWord#, geWord#, gtWord#
  , not#, and#, or#, xor#
  , clz#
  )
import qualified GHC.Exts as GHC
import GHC.Num hiding (quotRemInteger)
import GHC.Real (fromIntegral)
import GHC.Stack (HasCallStack)

import Control.Applicative (Applicative (..))
import Control.Exception (assert)
import Data.Bool
import Data.Char hiding (chr)
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.Ord
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Tuple
import Data.Word

import Text.Read
    ( Read (readPrec, readList, readListPrec)
    , readListDefault, readListPrecDefault
    )
import Text.Show

#if __GLASGOW_HASKELL >= 902
import GHC.Exts ( Word8#, word8ToWord#, wordToWord8# )

#else
import qualified GHC.Exts as Exts ( narrow8Word# )

type Word8# = Word#

word8ToWord# :: Word8# -> Word#
word8ToWord# w8 = w8

wordToWord8# :: Word# -> Word8#
wordToWord8# = Exts.narrow8Word#
{-# INLINE wordToWord8# #-}
#endif

chr :: Int -> Char
{-^ Contvert an 'Int' to a 'Char', replacing invalid 'Int' values with the Unicode replacement character \xFFFD.
-}
chr i@( GHC.I# i# )
    | i < 0
    ||  (0xD800 <= i && i <= 0xDFFF)  -- UTF-16 surrogates
    || i >= 0x10FFFF -- max Unicode code point
    = '\xFFFD'
    | otherwise
    = GHC.C# (chr# i# )
