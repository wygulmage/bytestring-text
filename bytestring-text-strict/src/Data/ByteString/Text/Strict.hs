{-# LANGUAGE NoImplicitPrelude
           , CPP
  #-}

module Data.ByteString.Text.Strict (
Text,
concat, append, empty, replicate, -- stimes and mtimesDefault are also part of the API, but must be imported from Data.Semigroup.
pack,
unpack, uncons, foldr, foldr', foldl, foldl',
null, length,
isPrefixOf, isSuffixOf, isInfixOf,
stripPrefix, stripSuffix,
) where

import Data.ByteString.Text.Strict.Internal
import Data.ByteString.Text.Builder.Internal.Utf8
import Data.ByteString.Text.Builder.Internal.Prelude
import Data.ByteString.Text.Builder.Internal.Search

import qualified Data.ByteString as BS

import GHC.Exts (build)
import Data.Coerce (coerce)

empty :: Text
empty = mempty
{-# INLINE empty #-}

append :: Text -> Text -> Text
append = mappend
{-# INLINE append #-}

concat :: [Text] -> Text
concat = mconcat
{-# INLINE concat #-}

replicate :: Int -> Text -> Text
replicate = stimes
{-# INLINE replicate #-}

foldl :: (b -> Char -> b) -> b -> Text -> b
foldl = foldlIndexLen (coerce BS.index) lengthWord8
{-# INLINABLE foldl #-}

foldl' :: (b -> Char -> b) -> b -> Text -> b
foldl' = foldl'IndexLen (coerce BS.index) lengthWord8
{-# INLINABLE foldl' #-}

foldr' :: (Char -> b -> b) -> b -> Text -> b
foldr' = foldr'IndexLen (coerce BS.index) lengthWord8
{-# INLINABLE foldr' #-}

null :: Text -> Bool
null = coerce BS.null
{-# INLINE null #-}

length :: Text -> Int
length = foldl' (\ n _ -> n + 1) 0
{-# NOTINLINE length #-}

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

intercalate :: Text -> [Text] -> Text
intercalate = coerce BS.intercalate
{-# INLINE intercalate #-}

-- isAscii :: Text -> Bool
-- isAscii txt = lengthWord8 txt == length txt
-- {-# INLINABLE isAscii #-}

copy :: Text -> Text
copy = coerce BS.copy
{-# INLINE copy #-}

-- unpackCString# = fromBuilder . Builder.cstringUtf8

------ Errors

-- emptyError :: [Char] -> a
-- emptyError op_name =
--     error ("Data.ByteString.Text." <> (op_name <> ": empty input"))
-- {-# NOINLINE emptyError #-}

{- Note: Redundant Char-based Functions
Instead of @'cons' :: Char -> Text -> Text@, use @('<>') :: Text -> Text -> Text@.
Instead of @'snoc' :: Text -> Char -> Text@, use @('<>') :: Text -> Text -> Text@.
Instead of @'replicate' :: Int -> Char -> Text@, use @stimes :: Int -> Text -> Text@, or @'mtimesDefault' :: Int -> Text -> Text@.

-}
