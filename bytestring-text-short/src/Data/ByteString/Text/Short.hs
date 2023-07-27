{-# LANGUAGE NoImplicitPrelude
           , CPP
           , BangPatterns
  #-}

{-| Strict UTF-8 Strings

'ShortText' is most suitable for strings that will be examined, but not edited, regardless of size. It has somewhat less overhead, but is less versatile, than 'Data.ByteString.Text.Strict.Text'.

Differences between 'Data.ByteString.Text.Strict.Text' and 'ShortText':
 + 'ShortText' cannot leave a large string in memory while only a tiny slice is used. (Slicing operations create entirely new 'ShortText's.)
 + 'ShortText' is (by default) unpinned. This means that the garbage collector can move it around to avoid fragmentation.
 - 'ShortText' does not provide incremental deconstruction like 'uncons', 'unsnoc', 'tail', or 'init'. If you want to consume a 'ShortText', consume it all at once.
-}

module Data.ByteString.Text.Short (
ShortText,
empty, concat, append,
pack,
unpack, foldr, foldr', foldl, foldl',
null, length,
isPrefixOf, isSuffixOf, isInfixOf,
cycleN, intercalate,
) where

import Data.ByteString.Text.Short.Internal
import qualified Data.ByteString.Text.Builder.Internal.Utf8 as Utf8

import qualified Data.ByteString.Short as BS

import GHC.Base hiding (empty, foldr)
import GHC.Num

import Data.Coerce (coerce)
import qualified Data.List as List
import Data.Word (Word8)

toShortByteString :: ShortText -> BS.ShortByteString
toShortByteString (SBS sbs) = sbs

foldl :: (a -> Char -> a) -> a -> ShortText -> a
foldl = Utf8.foldlIndexLen (coerce BS.index) lengthWord8
{-# INLINEABLE foldl #-}

foldl' :: (a -> Char -> a) -> a -> ShortText -> a
foldl' = Utf8.foldl'IndexLen (coerce BS.index) lengthWord8
{-# INLINEABLE foldl' #-}

foldr' :: (Char -> a -> a) -> a -> ShortText -> a
foldr' = Utf8.foldr'IndexLen (coerce BS.index) lengthWord8
{-# INLINEABLE foldr' #-}

null :: ShortText -> Bool
null = coerce BS.null
{-# INLINE null #-}

length :: ShortText -> Int
length = foldl' (\ len _ -> len + 1) 0
{-# NOTINLINE length #-}

isInfixOf :: ShortText -> ShortText -> Bool
#if MIN_VERSION_bytestring(0,11,3)
isInfixOf = coerce BS.isInfixOf
{-# INLINE isInfixOf #-}
#else
isInfixOf pre = List.isInfixOf (unpackWord8 pre) . unpackWord8
#endif


intercalate :: ShortText -> [ShortText] -> ShortText
#if MIN_VERSION_bytestring(0,11,3)
intercalate = coerce BS.intercalate
{-# INLINE intercalate #-}
#else
intercalate sep = concat . List.intersperse sep
{-# INLINABLE intercalate #-}
#endif

unpackWord8 :: ShortText -> [Word8]
{-^ Some of the definitions here use Data.List functions. This is an attempt to ensure fusion by using 'build'.
-}
#if MIN_VERSION_bytestring(0,11,3)
unpackWord8 = coerce BS.unpack
#else
unpackWord8 = \ sbs -> build (\ f z -> foldrWord8 f z sbs)
{-# INLINE [~0] unpackWord8 #-}
#endif

foldrWord8 :: (Word8 -> b -> b) -> b -> ShortText -> b
#if MIN_VERSION_bytestring(0,11,3)
foldrWord8 = coerce SBS.foldr
{-# INLINE foldrWord8 #-}
#else
foldrWord8 f z (SBS sbs) =
  let
    !len = BS.length sbs
    go_foldrW8 !i
        | i < len
        = BS.index sbs i `f` go_foldrW8 (i + 1)
        | otherwise
        = z
  in go_foldrW8 0
{-# INLINE [0] foldrWord8 #-}
#endif

{- Note: bytestring has had ShortByteString since version 0.10.4.0.

The original API consisted of only
 * toShort, fromShort, pack, empty
 * unpack, index
 * null, length

Version 0.10.9.0 adds
 * packCString, packCStringLen
 * useAsCString, useAsCStringLen

Version 0.11.0.0 adds
 * indexMaybe, (!?)

Version 0.11.3.0 adds (ignoring irrelevant operations)
 * concat, replicate, unfoldr, unfoldrN
 * head, last, foldl, foldl', foldl1, foldl1', foldr, foldr', foldr1, foldr1', take, takeEnd, drop, dropEnd, splitAt, breakSubstring
 * isValidUtf8, all, any, isInfixOf, isPrefixOf, isSuffixOf
 * intercalate, stripPrefix, stripSuffix
-}
