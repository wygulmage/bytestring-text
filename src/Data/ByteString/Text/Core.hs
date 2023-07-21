{-# LANGUAGE NoImplicitPrelude
           , ScopedTypeVariables
           , GADTs
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , TypeFamilies
  #-}

module Data.ByteString.Text.Core (
Text,
--- Construct:
pack,
empty,
append,
concat,
decodeUtf8, decodeUtf8Lenient,
--- Consume:
unpack, encodeUtf8,
--- Summarize:
null,
isPrefixOf, isSuffixOf, isInfixOf,
--- Transform:
stripPrefix, stripSuffix,
-- replace,
--- Low-level:
copy,
-- unpackCString#,
--- Helpers:
emptyError
{- Data.Text API (for comparison):
--- Type
Text,

--- Text-based (all of these should be provided by this module)
empty, append, concat,
replicate,
null,
isAscii,
isPrefixOf, isSuffixOf, isInfixOf,
stripPrefix, stripSuffix, commonPrefixes,
strip, stripStart, stripEnd,
intercalate,
replace,
breakOn, breakOnEnd, breakOnAll,
splitOn,
lines, unlines,
unwords, -- unwords = intercalate " "
copy, unpackCString#, unpackCStringAscii#,

--- non-Unicode-segmenting
words,

--- somewhat Unicode-aware
toCaseFold, toLower, toUpper, toTitle,

--- Char-based producers (some of these should be provided by this module)
pack, unpack, singleton,
cons, snoc,
unfoldr, unfoldrN,

--- single-Char consumers (these should be provided by the Char submodule)
uncons, unsnoc, head, last, tail, init, inits, tails,

--- all-Char consumers (these should be provided by the Char submodule)
foldl, foldl', foldl1, foldl1', foldr, foldr', foldr1,

--- Char-based summaries ("safe" but misleading; these should be provided by the Char submodule)
length, compareLength, measureOff,
any, all, maximum, minimum,
index, findIndex, count,
find, elem,

--- Char-based transformers (can easily garble perceived text; some should be provided by the Char submodule; others may be dropped completely)
map, concatMap,
intersperse, transpose, reverse,  -- E.g. reverse can put an accent on the wrong glyph.
justifyLeft, justifyRight, center,  -- These rely on Char count rather than glyph width, aren't bidirectional-aware. Also they pad rather than justifying!
scanl, scanl1, scanr, scanr1,
mapAccumL, mapAccumR,
take, takeEnd, drop, dropEnd, takeWhile, takeWhileEnd, dropWhile, dropWhileEnd, dropAround,  -- can easily split a base from its modifiers
splitAt, break,  -- can easily split a base and its modifiers
span, spanM, spanEndM,  -- can easily split a base from its modifiers
group, groupBy,  -- will split a base from its modifiers
split, chunksOf,  -- can easily split a base from its modifiers
filter, partition,  -- can easily remove a base while leaving its modifiers (which will then apply to the previous Char)
zip, zipWith,
-}
{- Data.Text.Encoding API (for comparison)
decodeUtf8Lenient, decodeUtf8',
decodeUtf8With,
decodeUtf8,
streamDecodeUtf8With,
decodeUtf8Chunk, decodeUtf8More,
validateUtf8Chunk, validateUtf8More,
encodeUtf8,
encodeUtf8Builder, encodeUtf8BuilderEscaped,

decodeASCIIPrefix, decodeASCII',
decodeASCII,

decodeLatin1,
decodeUtf16LE, decodeUtf16BE, decodeUtf32LE, decodeUtf32BE,
decodeUtf16LEWith, decodeUtf16BEWith, decodeUtf32LEWith, decodeUtf32BEWith,

encodeUtf16LE, encodeUtf16BE, encodeUtf32LE, encodeUtf32BE,
-}
) where

{-| This is the simple core of 'ByteString'-backed text. Rather than exporting the full 'Data.Text' API, it only exports class instances and the minimal API needed to generically define the full 'Data.Text' API.

Security model:
Text encodings are a fantastic attack vector. For a library like this, we want to make guarantees like "any 'Text' is valid UTF-8" and "processing invalid UTF-8 won't crash your program unless you explicitly demand that it does". "Escape hatches" like the 'UnsafeFromByteString' constructor or functions like 'dropWord8' violate this.
-}

import Data.ByteString.Text.Core.Internal
import Data.ByteString.Text.Core.Internal.Prelude

import qualified Data.ByteString as BS

import Data.Coerce (coerce)

{- doctest
$setup
>>> import Test.QuickCheck
>>> import Control.Applicative
-}


isPrefixOf :: Text -> Text -> Bool
{-^ O(length prefix)
>>> "pre" `isPrefixOf` "prefix"
True
-}
isPrefixOf = coerce BS.isPrefixOf
{-# INLINE isPrefixOf #-}

isSuffixOf :: Text -> Text -> Bool
{-^ O(length suffix)
>>> "fix" `isSuffixOf` "prefix"
True
-}
isSuffixOf = coerce BS.isSuffixOf
{-# INLINE isSuffixOf #-}

isInfixOf :: Text -> Text -> Bool
{-^ O(n * m)
>>> "fi" `isInfixOf` "prefix"
True

>>>> "y" `isInfixOf` "haystack"
True

If @elem :: Char -> Text -> Bool@ were defined, it would be @elem c = isInfixOf ('singleton' c)@.
-}
isInfixOf = coerce BS.isInfixOf
{-# INLINE isInfixOf #-}

stripPrefix :: Text -> Text -> Maybe Text
{-^ O(length prefix)
>>> stripPrefix "pre" "prefix"
Just "fix"

prop> \ pre txt -> if pre `isPrefixOf` txt then stripPrefix pre txt == Just (drop (length pre) txt) else stripPrefix pre txt == Nothing

prop> \ pre text -> case stripPrefix pre txt of{ Just txt' -> txt == append pre txt' ; Nothing -> True }
-}
stripPrefix = coerce BS.stripPrefix
{-# INLINE stripPrefix #-}
stripSuffix :: Text -> Text -> Maybe Text
{-^ O(length suffix)
-}
stripSuffix = coerce BS.stripSuffix
{-# INLINE stripSuffix #-}

-- isAscii :: Text -> Bool
-- isAscii txt = lengthWord8 txt == length txt
-- {-# INLINABLE isAscii #-}

copy :: Text -> Text
copy = coerce BS.copy
{-# INLINE copy #-}

-- unpackCString# = fromBuilder . Builder.cstringUtf8

------ Errors

emptyError :: [Char] -> a
emptyError op_name =
    error ("Data.ByteString.Text." <> (op_name <> ": empty input"))
{-# NOINLINE emptyError #-}

