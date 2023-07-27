{-# LANGUAGE NoImplicitPrelude
           , CPP
           , BangPatterns
  #-}

module Data.ByteString.Text.Short (
ShortText,
empty, concat, append,
pack,
unpack, foldr,
isPrefixOf, isSuffixOf, isInfixOf,
replicate, intercalate,
) where

import Data.ByteString.Text.Short.Internal

import qualified Data.ByteString.Short as SBS

import GHC.Base hiding (empty, foldr)
import GHC.Num

import Data.Coerce (coerce)
import qualified Data.List as List
import Data.Word (Word8)

toShortByteString :: ShortText -> SBS.ShortByteString
toShortByteString (SBS sbs) = sbs

null :: ShortText -> Bool
null = coerce SBS.null
{-# INLINE null #-}

isPrefixOf :: ShortText -> ShortText -> Bool
#if MIN_VERSION_bytestring(0,11,3)
isPrefixOf = coerce SBS.isPrefixOf
{-# INLINE isPrefixOf #-}
#else
isPrefixOf pre = List.isPrefixOf (unpackWord8 pre) . unpackWord8
#endif

isSuffixOf :: ShortText -> ShortText -> Bool
#if MIN_VERSION_bytestring(0,11,3)
isSuffixOf = coerce SBS.isSuffixOf
{-# INLINE isSuffixOf #-}
#else
isSuffixOf (SBS pre) (SBS txt) =
    n <= SBS.length txt
    &&  go_suffixOf (n - 1)
  where
    !n = SBS.length pre
    !txt_off = SBS.length txt - n
    go_suffixOf !i =
        i < 0  ||
        (SBS.index pre i == SBS.index txt (i + txt_off)  &&  go_suffixOf (i - 1))
{-# NOTINLINE isSuffixOf #-}
#endif

isInfixOf :: ShortText -> ShortText -> Bool
#if MIN_VERSION_bytestring(0,11,3)
isInfixOf = coerce SBS.isInfixOf
{-# INLINE isInfixOf #-}
#else
isInfixOf pre = List.isInfixOf (unpackWord8 pre) . unpackWord8
#endif


intercalate :: ShortText -> [ShortText] -> ShortText
#if MIN_VERSION_bytestring(0,11,3)
intercalate = coerce SBS.intercalate
{-# INLINE intercalate #-}
#else
intercalate sep = concat . List.intersperse sep
{-# INLINABLE intercalate #-}
#endif

unpackWord8 :: ShortText -> [Word8]
{-^ Some of the definitions here use Data.List functions. This is an attempt to ensure fusion by using 'build'.
-}
#if MIN_VERSION_bytestring(0,11,3)
unpackWord8 = coerce SBS.unpack
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
    !len = SBS.length sbs
    go_foldrW8 !i
        | i < len
        = SBS.index sbs i `f` go_foldrW8 (i + 1)
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
