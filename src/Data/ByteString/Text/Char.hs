{-# LANGUAGE NoImplicitPrelude
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , TypeFamilies
           , CPP
           , MultiWayIf
  #-}

{-| This is the 'Char'-based API for 'Data.ByteString.Text.Core.Text'
Using 'Char' as the basic unit of 'Text' is problematic. It's unaware of higher level units that should not be split up. For example, 'reverse' will change @"a\\x301"@ (a with combining acute) to @"\\x301\\x61"@ (combining acute followed by a).

The only thing that makes this "safe" is that the results are still valid UTF-8 even though they are nonsense, and you can safely use it to escape, add, and remove control characters.

Think of this being equivalent to using 8-bit chars in C. It will work fine for a lot of things, but you're always running the risk of garbling text.
-}

module Data.ByteString.Text.Char (
--- Construct:
pack,
singleton,
--- Consume:
unpack,
uncons, splitAt, span,
foldl, foldl',
foldr, foldr',
unsnoc,
head, last,
tail, init,
drop, dropWhile, dropEnd, dropWhileEnd,
take, takeWhile, takeEnd, takeWhileEnd,
--- Summarize
compareLength, length, -- measureOff,
--- Transform
map, concatMap,
filter,
reverse,

{- For comparison, here is the Char-based part of the Data.Text API:

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
) where

import Data.ByteString.Text.Core
import Data.ByteString.Text.Core.Internal
import Data.ByteString.Text.Core.Internal.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS (unsafePackLenBytes)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder

import GHC.Exts (build)

import qualified Data.List as List


-- singleton :: Char -> Text
-- {-^ O(1)
-- >>> singleton 'a'
-- "a"
-- -}
-- -- singleton = toTextWith 4 . charUtf8  -- Using the Builder-based definition leads to massive code size blowup.
-- singleton !c = UnsafeFromByteString $ case charBytes c of
--     CharBytes1 w0 -> BS.singleton w0
--     CharBytes2 w0 w1 -> BS.unsafePackLenBytes 2 (w0 : w1 : [])
--     CharBytes3 w0 w1 w2 -> BS.unsafePackLenBytes 3 (w0 : w1 : w2 : [])
--     CharBytes4 w0 w1 w2 w3 -> BS.unsafePackLenBytes 4 (w0 : w1 : w2 : w3 : [])

map :: (Char -> Char) -> Text -> Text
map f = pack . List.map f . unpack
{-# INLINE [~0] map #-}

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . List.map f . unpack
{-# INLINE [~0] concatMap #-}


filter :: (Char -> Bool) -> Text -> Text
{-^ O(n) Remove all 'Char's that do not match the predicate. -}
filter p txt = case span p txt of
    (txt1, txt2)
        | null txt2
        -> txt1  -- Nothing was filtered out.
--        | null txt1 -> filter p (tail txt2)
        | otherwise
        -> toTextWith
            (min (lengthWord8 txt - 1) defaultChunkSize)
            (fromText txt1 <> go txt2)
  where
    go txt2 = case span p txt2 of
        (txt1', txt2')
            | null txt2' -> fromText txt1'
            | otherwise -> fromText txt1' <> go (tail txt2')



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

splitAt# :: Int -> Text -> (# Text, Text #)
splitAt# !n !cs = (# tcs, dcs #)
  where
    !dcs = drop n cs
    !tcs = takeWord8 (lengthWord8 cs - lengthWord8 dcs) cs
{-# INLINABLE splitAt# #-}

span :: (Char -> Bool) -> Text -> (Text, Text)
{-^ O(n)
@span p txt = ('takeWhile' p txt, 'dropWhile' p txt)@, but it is significantly more efficient.

prop> \ p txt -> span (apply p) txt == (takeWhile (apply p) txt, dropWhile (apply p) txt)
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
{-^ O(n)
@drop n txt@ is @txt@ without the first @n@ 'Char's. If @n >= 'length' txt@, the result is 'empty'.
-}
drop !n !cs
    | n <= 0 = cs
    | null cs = empty  -- Avoid creating extra empty values.
    | otherwise = drop (n - 1) (unsafeTail cs)
{-# NOTINLINE drop #-} -- recursive

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

dropWhile :: (Char -> Bool) -> Text -> Text
{-^ O(n)
@dropWhile p txt@ removes the longest prefix of 'Char's that satisfy @p@ from @txt@.
-}
dropWhile p = dropWhile_loop
  where
    dropWhile_loop !cs =
        case uncons cs of
            Just (c, cs')
                | p c -> dropWhile_loop cs'
                | otherwise -> cs
            Nothing -> empty  -- Avoid creating extra empty values.
{-# INLINE [~0] dropWhile #-}

dropWhileEnd :: (Char -> Bool) -> Text -> Text
{-^ O(n)
@dropWhileEnd p txt@ removes the longest suffix of 'Char's that satisfy @p@ from @txt@.
-}
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
{-^ O(n)
@take n txt@ is the first @n@ 'Char's of @txt@ if @'length' txt >= n@; otherwise it is @txt@.
-}
take !n !cs = case splitAt# n cs of (# cs', _ #) -> cs'
{-# NOTINLINE take #-}

takeEnd :: Int -> Text -> Text
{-^ O(n)
@takeEnd n txt@ is the last @n@ 'Char's of @txt@ of @'length' txt >= n@; otherwise it is @txt@.
-}
takeEnd !n !cs = dropWord8 (lengthWord8 (dropEnd n cs)) cs
{-# NOTINLINE takeEnd #-}

takeWhile :: (Char -> Bool) -> Text -> Text
{-^ O(n)
@takeWhile p txt@ is @txt@'s longest prefix of 'Char's that satisfy @p@.
-}
takeWhile p !cs = case span# p cs of (# cs', _ #) -> cs'
{-# INLINE [~0] takeWhile #-}

takeWhileEnd :: (Char -> Bool) -> Text -> Text
{-^ O(n)
@takeWhileEnd p txt@ is @txt@'s longest suffix of 'Char's that satisfy @p@.

>>> takeWhileEnd Char.isLower "Period is not lower."
""

>>> takeWhileEnd (not . Char.isUpper) "Period is not upper."
"eriod is not upper."
-}
takeWhileEnd !p !cs = dropWord8 (lengthWord8 (dropWhileEnd p cs)) cs
{-# INLINE [~0] takeWhileEnd #-}


foldl :: (b -> Char -> b) -> b -> Text -> b
{-^ O(n)
@foldl f z txt@ uses @f@ stream the 'Char's of @txt@, starting with the last @Char@ and ending with @z@.

@foldl (_ c -> c) ('emptyError' "last")@ = @'last'@.
-}
foldl f z = loop
  where
    loop !cs =
        case unsnoc cs of
            Nothing -> z
            Just (cs', c) -> loop cs' `f` c
{-# INLINABLE foldl #-}

foldl' :: (b -> Char -> b) -> b -> Text -> b
{-^ O(n) (strict)
@foldl' f z txt@ uses @f@ to accumulate the 'Char's of @txt@ onto @z@, starting with the first @Char@.
-}
foldl' f = loop
  where
    loop z !cs =
      case uncons cs of
          Nothing -> z
          Just (c, cs') -> let !z' = f z c in loop z' cs'
{-# INLINABLE foldl' #-}

-- foldr is defined in Internal.

foldr' :: (Char -> b -> b) -> b -> Text -> b
{-^ O(n) (strict)
@foldr' f z txt@ uses @f@ to accumulate the 'Char's of @txt@ onto @z@, starting with the last @Char@.
-}
foldr' f = loop
  where
    loop z !cs =
        case unsnoc cs of
            Nothing -> z
            Just (cs', c) -> let !z' = f c z in loop z' cs'
{-# INLINABLE foldr' #-}

reverse :: Text -> Text
{-^ O(n)
Reverse the order of the 'Char's of the 'Text'
>>> reverse "D:"
":D"

I would give an example here of a acute folowed by b reversing to b acute followed by a, but it's bugging out my emacs in WSL.
-}
reverse cs = -- Could first test whether cs is shorter than 2 Chars.
    UnsafeFromByteString
        (BS.unsafePackLenBytes
            (lengthWord8 cs)
            (build (\ cons nil ->
                foldl
                    (\ cs' c -> foldrCharBytes cons cs' c)
                     nil
                     cs)))
{-# INLINE [~0] reverse #-}
