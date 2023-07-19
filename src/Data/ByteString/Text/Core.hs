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
) where

{- This is the simple core of 'ByteString'-backed text. Rather than exporting the full 'Data.Text' API, it only exports class instances and the minimal API needed to generically define the full 'Data.Text' API.
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

{-
newtype GraphemeCluster = UnsafeFromText Text
  deriving newtype
    ( Ord, Eq
    , NFData
    )
-}

encodeUtf8 :: Text -> BS.ByteString
{-^ O(1) Convert 'Text' to a UTF-8-encoded 'ByteString' -}
encodeUtf8 (UnsafeFromByteString bs) = bs

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

copy :: Text -> Text
copy = coerce BS.copy
{-# INLINE copy #-}

-- unpackCString# = fromBuilder . Builder.cstringUtf8

------ Errors

emptyError :: [Char] -> a
emptyError op_name =
    error ("Data.ByteString.Text." <> (op_name <> ": empty input"))
{-# NOINLINE emptyError #-}

