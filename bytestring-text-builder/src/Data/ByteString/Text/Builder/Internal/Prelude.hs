{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
  #-}

{-| This module exists to avoid name clashes created by importing 'Prelude' unqualified.
-}
module Data.ByteString.Text.Builder.Internal.Prelude (
($!),
HasCallStack, assert,
module GHC.Enum,
module GHC.Err,
module GHC.Exts, chr'#,
#if !(__GLASGOW_HASKELL >= 902)
Word8#, wordToWord8#, word8ToWord#, eqWord8#,
#endif
module GHC.Num,
module GHC.Real,
module GHC.Word,
module Control.Applicative,
module Data.Bits,
module Data.Bool,
module Data.Char, chr, chr',
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
module Text.Read,
module Text.Show,
) where

import GHC.Base (($!))
import GHC.Enum
import GHC.Err (error, errorWithoutStackTrace, undefined)
-- Bring basic unlifted operators into scope when MagicHash is enabled:
import GHC.Exts
  ( Char( C# ), Char#
  , eqChar#, neChar#, leChar#, ltChar#, geChar#, gtChar#
  , chr#
  , Int( I# ), Int#
  , negateInt#, ( *# ), ( +# ), ( -# )
  , ( ==# ), ( /=# ), ( <=# ), ( <# ), ( >=# ), ( ># )
  , notI#, andI#, orI#, xorI#
  , Word( W# ), Word#
  , eqWord#, neWord#, leWord#, ltWord#, geWord#, gtWord#
  , not#, and#, or#, xor#
  , clz#
  , isTrue#
  )
import qualified GHC.Exts as GHC
import GHC.Num hiding (quotRemInteger)
import GHC.Real
import GHC.Stack (HasCallStack)
import GHC.Word

import Control.Applicative (Applicative (..))
import Control.Exception (assert)
import Data.Bits hiding (bitSize)
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

import Text.Read
    ( Read (readPrec, readList, readListPrec)
    , readListDefault, readListPrecDefault
    )
import Text.Show

#if __GLASGOW_HASKELL >= 902
import GHC.Exts ( Word8#, word8ToWord#, wordToWord8#, eqWord8# )

#else
import qualified GHC.Exts as Exts ( narrow8Word# )

type Word8# = Word#

word8ToWord# :: Word8# -> Word#
word8ToWord# w8 = w8

wordToWord8# :: Word# -> Word8#
wordToWord8# = Exts.narrow8Word#
{-# INLINE wordToWord8# #-}

eqWord8# :: Word8# -> Word8# -> Int#
eqWord8# = eqWord#
{-# INLINE eqWord8# #-}
#endif


chr :: Int -> Char
{-^ Contvert an 'Int' to a 'Char', replacing invalid 'Int' values with the Unicode replacement character \xFFFD.
-}
chr ( GHC.I# i# ) = chr' ( GHC.W# ( GHC.int2Word# i# ))
{-# INLINE chr #-}

chr' :: Word -> Char
{-^ Convert a 'Word' to a 'Char', replacing invalid 'Int' values with the Unicode replacement character \xFFFD.
-}
chr' w@( GHC.W# w# )
    | w > 0x10FFFF -- max Unicode code point
    || (0xD800 <= w  &&  w <= 0xDFFF) -- UTF-16 surrogates
    = '\xFFFD'
    | otherwise
    = GHC.C# ( chr'# w# )
{-# INLINABLE chr' #-}

chr'# :: Word# -> Char#
chr'# w = chr# (GHC.word2Int# w)
{-# INLINE chr'# #-}
