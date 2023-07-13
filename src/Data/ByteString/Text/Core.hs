{-# LANGUAGE NoImplicitPrelude
           , ScopedTypeVariables
           , GADTs
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , TypeFamilies
  #-}

module Data.ByteString.Text.Core (
Text, encodeUtf8,
-- Construct:
pack,
singleton,
empty,
append,
concat, concatMap,
-- Consume:
unpack,
uncons, unsnoc,
head, last,
tail, init,
drop, dropWhile, dropEnd, dropWhileEnd,
take, takeWhile, takeEnd, takeWhileEnd,
splitAt, span,
foldl, foldl',
foldr, foldr',
-- Summarize:
null, compareLength, length, measureOff,
isPrefixOf, isSuffixOf, isInfixOf,
-- Transform:
reverse,
stripPrefix, stripSuffix,
-- Low-level:
copy,
-- unpackCString#,
-- Helpers:
emptyError
) where

{- This is the simple core of 'ByteString'-backed text. Rather than exporting the full 'Data.Text' API, it only exports class instances and the minimal API needed to generically define the full 'Data.Text' API.
-}

import Data.ByteString.Text.Core.Internal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (unsafePackLenBytes)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import GHC.Base hiding (empty, foldr)
import GHC.Num (Num (..))
import qualified Data.List as List
import Data.Tuple
-- import Data.Coerce (coerce) -- provided by GHC.Base

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

compareLength :: Text -> Int -> Ordering
{-^ O(n) Compare the length of the 'Text' to the 'Int'. Only reads as much of the 'Text' as is needed to perform the comparison.

prop> \ txt i -> compareLength txt i == compare (length txt) i
-}
compareLength !cs !n
    | null cs = compare 0 n
    | otherwise = compareLength (unsafeTail cs) (n - 1)
{-# NOINLINE compareLength #-}

length :: Text -> Int
{-^ O(n) (strict) the number of 'Char's in the 'Text' -}
length = foldl' (\ acc _ -> acc + 1) 0
{-# NOTINLINE length #-}

head :: Text -> Char
{-^ O(1) @head txt@ is the first 'Char' of @txt@. It is an error if @txt@ is 'null'. -}
head !cs
    | null cs   = emptyError "head"
    | otherwise = unsafeHead cs
{-# INLINE head #-}

last :: Text -> Char
{-^ O(1) @head txt@ is the last 'Char' of @txt@. It is an error if @txt@ is 'null'. -}
last !cs = case unsnoc cs of
    Nothing -> emptyError "last"
    Just (_, c) -> c
{-# INLINABLE last #-}

tail :: Text -> Text
{-^ O(1) Drop the first 'Char' from the 'Text'. Unlike 'drop', @tail@ throws an error if the 'Text' is empty.
-}
tail !cs
    | null cs   = emptyError "tail"
    | otherwise = unsafeTail cs
{-# INLINE tail #-}

init :: Text -> Text
{-^ O(1) Drop the last 'Char' from the 'Text'. Unlike 'dropEnd', @init@ throws an error if the 'Text' is empty.
-}
init !cs = case unsnoc cs of
    Nothing -> emptyError "init"
    Just (cs', _) -> cs'
{-# INLINABLE init #-}

splitAt :: Int -> Text -> (Text, Text)
{-^ O(n)

prop> \ n txt -> splitAt n txt == (take n txt, drop n txt)
-}
splitAt !n !cs = case splitAt# n cs of (# tcs, dcs #) -> (tcs, dcs)
{-# INLINE splitAt #-}

-- Just a bit of premature optimization here.
splitAt# :: Int -> Text -> (# Text, Text #)
splitAt# !n !cs = (# tcs, dcs #)
  where
    !dcs = drop n cs
    !tcs = takeWord8 (lengthWord8 cs - lengthWord8 dcs) cs
{-# INLINABLE splitAt# #-}

span :: (Char -> Bool) -> Text -> (Text, Text)
{-^ O(n)
-}
span p !cs = case span# p cs of (# tcs, dcs #) -> (tcs, dcs)
{-# INLINE span #-}

span# :: (Char -> Bool) -> Text -> (# Text, Text #)
span# p !cs = (# tcs, dcs #)
  where
    !dcs = dropWhile p cs
    !tcs = takeWord8 (lengthWord8 cs - lengthWord8 dcs) cs
{-# INLINABLE span# #-}

drop :: Int -> Text -> Text
drop !n !cs
    | n <= 0 = cs
    | null cs = empty  -- Avoid creating extra empty values.
    | otherwise = drop (n - 1) (unsafeTail cs)
{-# NOTINLINE drop #-} -- recursive

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p = dropWhile_loop
  where
    dropWhile_loop !cs =
        case uncons cs of
            Just (c, cs')
                | p c -> dropWhile_loop cs'
                | otherwise -> cs
            Nothing -> empty  -- Avoid creating extra empty values.
{-# INLINE [~0] dropWhile #-}

dropEnd :: Int -> Text -> Text
{-^ O(n) @dropEnd n txt@ is all except the last @n@ 'Char's of @txt@ if @'length' txt >= n@; otherwise it is 'empty'.

prop> \ n txt -> length (drop n txt) == (length txt - n) || (n > length txt  &&  null (drop n txt))
-}
dropEnd !n !cs
    | n <= 0
    = cs
    | otherwise
    = case unsnoc cs of
        Just (cs', _) -> dropEnd (n - 1) cs'
        Nothing -> empty  -- Avoid creating extra empty values.
{-# NOTINLINE dropEnd #-} -- recursive

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = dropWhileEnd_loop
  where
    dropWhileEnd_loop !cs =
      case unsnoc cs of
          Just (cs', c)
              | p c -> dropWhileEnd_loop cs'
              | otherwise -> cs
          Nothing -> empty  -- Avoid creating extra empty values.
{-# INLINE [~0] dropWhileEnd #-} -- recursive

take :: Int -> Text -> Text
{-^ O(n) @take n txt@ is the first @n@ 'Char's of @txt@ if @'length' txt >= n@; otherwise it is @txt@. -}
take !n !cs = case splitAt# n cs of (# cs', _ #) -> cs'
{-# NOTINLINE take #-}

takeWhile :: (Char -> Bool) -> Text -> Text
{-^ O(n)
-}
takeWhile p !cs = case span# p cs of (# cs', _ #) -> cs'
{-# INLINE [~0] takeWhile #-}

takeEnd :: Int -> Text -> Text
{-^ O(n) @takeEnd n txt@ is the last @n@ 'Char's of @txt@ of @'length' txt >= n@; otherwise it is @txt@. -}
takeEnd !n !cs = dropWord8 (lengthWord8 (dropEnd n cs)) cs
{-# NOTINLINE takeEnd #-}

takeWhileEnd :: (Char -> Bool) -> Text -> Text
{-^ O(n)
@takeWhileEnd p txt@ drops up to and including the last 'Char' of 'txt' that does not satisfy @p@, leaving only the characters at the end of 'txt' that satisfy @p@.

>>> takeWhileEnd Char.isLower "Period is not lower."
""

>>> takeWhileEnd (not . Char.isUpper) "Period is not upper."
"eriod is not upper."
-}
takeWhileEnd !p !cs = dropWord8 (lengthWord8 (dropWhileEnd p cs)) cs
{-# INLINE [~0] takeWhileEnd #-}


foldl :: (b -> Char -> b) -> b -> Text -> b
{-^ O(n) -}
foldl f z = loop
  where
    loop !cs =
        case unsnoc cs of
            Nothing -> z
            Just (cs', c) -> loop cs' `f` c
{-# INLINABLE foldl #-}

foldl' :: (b -> Char -> b) -> b -> Text -> b
{-^ O(n) (strict) -}
foldl' f = loop
  where
    loop z !cs =
      case uncons cs of
          Nothing -> z
          Just (c, cs') -> let !z' = f z c in loop z' cs'
{-# INLINABLE foldl' #-}

-- foldr is defined in Internal.

foldr' :: (Char -> b -> b) -> b -> Text -> b
{-^ O(n) (strict) -}
foldr' f = loop
  where
    loop z !cs =
        case unsnoc cs of
            Nothing -> z
            Just (cs', c) -> let !z' = f c z in loop z' cs'
{-# INLINABLE foldr' #-}

reverse :: Text -> Text
reverse cs = -- Could first test whether cs is shorter than 2 Chars.
    UnsafeFromByteString
        (BS.unsafePackLenBytes
            (lengthWord8 cs)
            (build (\ cons nil ->
                foldl
                    (\ cs' c ->
                        foldrCharBytes
                            (\ w8 w8s' -> w8 `cons` w8s')
                            cs'
                            c)
                     nil
                     cs)))

isPrefixOf :: Text -> Text -> Bool
{-^ O(length prefix)
>>> "pre" `isPrefixOf` "prefix"
True

>>> "fix" `isSuffixOf` "prefix"
True

>>> "fi" `isInfixOf` "prefix"
True

-}
isPrefixOf = coerce BS.isPrefixOf
{-# INLINE isPrefixOf #-}

isSuffixOf :: Text -> Text -> Bool
{-^ O(length suffix)
>>> "pre" `isPrefixOf` "prefix"
True

>>> "fix" `isSuffixOf` "prefix"
True

>>> "fi" `isInfixOf` "prefix"
True

-}
isSuffixOf = coerce BS.isSuffixOf
{-# INLINE isSuffixOf #-}

{-^ O(n * m)
>>> "fi" `isInfixOf` "prefix"
True
-}
isInfixOf :: Text -> Text -> Bool
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

singleton :: Char -> Text
{-^ O(1)
>>> singleton 'a'
"a"
-}
-- singleton = toTextWith 4 . charUtf8  -- Using the Builder-based definition leads to massive code size blowup.
singleton !c = UnsafeFromByteString $ case charBytes c of
    CharBytes1 w0 -> BS.singleton w0
    CharBytes2 w0 w1 -> BS.unsafePackLenBytes 2 (w0 : w1 : [])
    CharBytes3 w0 w1 w2 -> BS.unsafePackLenBytes 3 (w0 : w1 : w2 : [])
    CharBytes4 w0 w1 w2 w3 -> BS.unsafePackLenBytes 4 (w0 : w1 : w2 : w3 : [])

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . List.map f . unpack
{-# INLINE [~0] concatMap #-}

copy :: Text -> Text
copy = coerce BS.copy
{-# INLINE copy #-}

-- unpackCString# = fromBuilder . Builder.cstringUtf8


------ Errors

emptyError :: [Char] -> a
emptyError op_name =
    error ("Data.ByteString.Text." <> (op_name <> ": empty input"))
{-# NOINLINE emptyError #-}

