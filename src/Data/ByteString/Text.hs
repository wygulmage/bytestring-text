{-# LANGUAGE NoImplicitPrelude
           , ScopedTypeVariables
           , BangPatterns
           , TypeFamilies
  #-}

module Data.ByteString.Text (
Text,
pack, unpack,
singleton,
empty,
uncons, unsnoc,
head, last,
tail, tails, init,
-- inits,

append, concat,
concatMap,
map,
-- intercalate, intersperse,
-- transpose,
reverse,
-- replace,
-- toCaseFold, toLower, toUpper, toTitle,

-- justifyLeft, justifyRight, center,

foldl, foldl', foldl1, foldl1',
foldr, foldr', foldr1, -- foldr1' is not provided.

null, length, compareLength,
any, all,
maximum, minimum,
isAscii,

-- scanl, scanl1,
-- scanr, scanr1,

-- mapAccumL,
-- mapAccumR,

-- replicate,
-- unfoldr,
--- unfoldrN,

take, takeEnd, takeWhile, takeWhileEnd,
drop, dropEnd, dropWhile, dropWhileEnd,dropAround,
strip, stripStart, stripEnd,
-- split, splitAt, splitOn,
-- chunksOf,
-- break, breakOn, breakOnEnd, breakOnAll,
span, -- spanM, spanEndM,
-- group, groupBy,
-- partition,

-- lines, unlines,
-- words, unwords,

isPrefixOf, isSuffixOf, isInfixOf,
stripPrefix, stripSuffix,
-- commonPrefixes,

filter,
find, findIndex,
elem,
index,
count,

zip, zipWith,

copy,
-- unpackCString#,
-- unpackCStringAscii#,

measureOff,
) where

import Data.ByteString.Text.Core
import qualified Data.ByteString as BS
import GHC.Exts
    ( IsString (..)
    , IsList (..)
    , Int
    )
import GHC.Num (Num (..))
import Data.Monoid (mconcat, mempty)
import Data.Semigroup ((<>))
import Data.Ord
import Data.Eq
import Data.Bool
import Data.Char
import Data.Function
import qualified Data.List as List
import Data.Maybe

-- Concrete versions of class methods:
empty :: Text
empty = mempty
{-# INLINE empty #-}

append :: Text -> Text -> Text
{-^ O(n + m) -}
append = (<>)
{-# INLINE append #-}

concat :: [Text] -> Text
concat = mconcat
{-# INLINE concat #-}

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . List.map f . unpack
{-# INLINE [~0] concatMap #-}

map :: (Char -> Char) -> Text -> Text
map f = pack . List.map f . unpack
{-# INLINE [~0] map #-}

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p cs = dropWhileEnd p (dropWhile p cs)

strip :: Text -> Text
strip cs = stripEnd (stripStart cs)

stripStart :: Text -> Text
stripStart = dropWhile isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd isSpace

tails :: Text -> [Text]
{-^ @tails txt@ is the list of @txt@ and @tails@ of @txt@'s 'tail', always ending with 'empty'. -}
tails cs
    | null cs = mempty : []
    | otherwise = cs : tails (unsafeTail cs)

filter :: (Char -> Bool) -> Text -> Text
filter p = fromList . List.filter p . toList
{-# INLINE [~0] filter #-}

--- Summaries:

find :: (Char -> Bool) -> Text -> Maybe Char
find p = foldr (\ c mc -> if p c then Just c else mc) Nothing
{-# INLINABLE [~0] find #-}

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p = loop 0
  where
    loop i cs =
        case uncons cs of
            Just (c, cs')
                | p c -> Just i
                | otherwise -> loop (i + 1) cs'
            Nothing -> Nothing
{-# NOTINLINE findIndex #-}

elem :: Char -> Text -> Bool
elem c cs = any ((==) c) cs
{-# NOTINLINE elem #-}

any, all :: (Char -> Bool) -> Text -> Bool
any p = foldr (\ c b -> p c || b) False
{-# INLINEABLE [~0] any #-}
all p = foldr (\ c b -> p c && b) True
{-# INLINEABLE [~0] all #-}

maximum :: Text -> Char
maximum cs = case foldl1'Maybe max cs of
    Just c -> c
    Nothing -> emptyError "maximum"
{-# NOTINLINE maximum #-}

minimum :: Text -> Char
minimum cs = case foldl1'Maybe min cs of
    Just c -> c
    Nothing -> emptyError "minimum"
{-# NOTINLINE minimum #-}

foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 f cs = case foldl1Maybe f cs of
    Just c -> c
    Nothing -> emptyError "foldl1"
{-# INLINABLE foldl1 #-}

foldl1' :: (Char -> Char -> Char) -> Text -> Char
foldl1' f cs = case foldl1'Maybe f cs of
    Just c -> c
    Nothing -> emptyError "foldl1'"
{-# INLINABLE foldl1' #-}

foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 f cs = case foldr1Maybe f cs of
    Just c -> c
    Nothing -> emptyError "foldr1"
{-# INLINABLE foldr1 #-}

foldl1Maybe :: (Char -> Char -> Char) -> Text -> Maybe Char
foldl1Maybe f cs = case unsnoc cs of
    Just (cs', c) -> Just (loop cs' c)
    Nothing -> Nothing
  where
    loop cs' c = case unsnoc cs' of
        Just (cs'', c') -> loop cs'' c' `f` c
        Nothing -> c

foldl1'Maybe :: (Char -> Char -> Char) -> Text -> Maybe Char
foldl1'Maybe f cs = case uncons cs of
    Just (c, cs') -> Just (loop c cs')
    Nothing -> Nothing
  where
    loop !acc !cs' = case uncons cs' of
        Just (c', cs'') -> loop (f acc c') cs''
        Nothing -> acc

foldr1Maybe :: (Char -> Char -> Char) -> Text -> Maybe Char
foldr1Maybe f cs = case uncons cs of
    Just (c, cs') -> Just (loop c cs')
    Nothing -> Nothing
  where
    loop c cs' = case uncons cs' of
        Nothing -> c
        Just (c', cs'') -> c `f` loop c' cs''

{-
foldr1'Maybe :: (Char -> Char -> Char) -> Text -> Maybe Char
foldr1'Maybe f cs = case unsnoc cs of
    Just (cs', c) -> let !cN = loop cs' c in Just cN
    Nothing -> Nothing
  where
    loop !cs' !acc = case unsnoc cs' of
        Just (cs'', c') -> loop cs'' (f c' acc)
        Nothing -> acc
 -}

index :: Text -> Int -> Char
index cs !i =
    case uncons cs of
        Just (c, cs')
            | i == 0 -> c
            | otherwise -> index cs' (i - 1)
        Nothing -> emptyError "index" -- TODO: Construct a real out-of-bounds error.
{-# NOINLINE index #-}

count :: Text -> Text -> Int
{-^ O(length xs * length ys) @count xs ys@ is the number of times the character sequence of @xs@ appears in @ys@. If @xs@ is null this throws an error.
-}
count xs ys
    | null xs
    = emptyError "count"
    | otherwise
    = count_loop 0 ys
  where
    -- TODO: Consider using a more efficient algorithm for this (e.g. Booyer-Moore or Knuth-Morris-Pratt type).
    count_loop n ys'
        | byteLength ys' < byteLength xs
        = n
        | otherwise
        = let
            !n'
                | isPrefixOf xs ys' = n + 1
                | otherwise = n
          in count_loop n' (unsafeTail ys')
{-# NOINLINE count #-}

zipWith :: (Char -> Char -> a) -> Text -> Text -> [a]
{-^ O(min (length xs) (length ys)) @zipWith f xs ys@ uses @f@ to combine the 'Char's of @xs@ and @ys@ and lists each result. When the end of the shorter 'Text' is reached, the list ends.
-}
zipWith f = loop
  where
    loop xs ys =
        case (uncons xs, uncons ys) of
            (Just (x, xs'), Just (y, ys')) ->
                f x y : loop xs' ys'
            _ ->
                []

zip :: Text -> Text -> [(Char, Char)]
zip = zipWith (,)

byteLength :: Text -> Int
{-^ O(1) -}
byteLength cs = BS.length (encodeUtf8 cs)
{-# INLINE byteLength #-}

