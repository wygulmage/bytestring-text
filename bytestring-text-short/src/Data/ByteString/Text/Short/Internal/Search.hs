{-# LANGUAGE NoImplicitPrelude
           , CPP
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , MagicHash
           , BangPatterns
           , UnboxedTuples
  #-}

{-# OPTIONS_HADDOCK not-home
  #-}

{-| functions for seaching and comparing ShortByteStrings
-}

module Data.ByteString.Text.Short.Internal.Search (
compareSlicesBS,
isInfixOfBS,
indicesBS,
indicesBrutalBS,
indicesTwoWayBS,
elemIndicesBS,
) where


import Data.ByteString.Text.Builder.Internal.Prelude
import Data.ByteString.Text.Builder.Internal.Search
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as IBS
import qualified GHC.Exts as GHC
-- import Debug.Trace (trace)
import qualified Data.List as List

------ The Result ------

eqSlicesBS ::
    BS.ShortByteString -> Int -> BS.ShortByteString -> Int -> Int ->
    Bool
eqSlicesBS
    ( IBS.SBS x ) ( I# off_x ) ( IBS.SBS y ) ( I# off_y ) ( I# n )
    = case GHC.compareByteArrays# x off_x y off_y n of
        sign -> isTrue# ( sign ==# 0# )
{-# INLINE eqSlicesBS #-}

compareSlicesBS ::
    BS.ShortByteString -> Int -> BS.ShortByteString -> Int -> Int ->
    Ordering
compareSlicesBS
    ( IBS.SBS x ) ( I# off_x ) ( IBS.SBS y ) ( I# off_y ) ( I# n )
    = case GHC.compareByteArrays# x off_x y off_y n of
        sign
            -- Matching the default 'compare' definition lets GHC make slightly better code for the *fixOf functions (and, in fact, Eq -- so let's do that.).
            -- There are "clever" nonbranching things we could do here, but GHC seems to do a better job with explicit branches that it can eliminate.
            | isTrue# ( sign ==# 0# ) -> EQ
            | isTrue# ( sign <=# 0# ) -> LT  -- could test with <#
            | otherwise               -> GT
{-# INLINE compareSlicesBS #-}

indicesBS :: BS.ShortByteString -> BS.ShortByteString -> [Int]
indicesBS needle
--    | l_needle == 1
--    = elemIndicesBS (BS.index needle 0)  -- There's barely any benefit.
    | l_needle == 0  -- The other functions are undefined for empty needles
    = (`List.take` [0..]) . BS.length
    | l_needle <= 32
    = indicesBrutalBS needle
    | otherwise
    = indicesTwoWayBS needle
  where
    !l_needle = BS.length needle
{-# INLINE indicesBS #-}

isInfixOfBS :: BS.ShortByteString -> BS.ShortByteString -> Bool
isInfixOfBS needle
--    | BS.length needle == 1
--    = elemBS (BS.index needle 0)  -- There's barely any benefit.
    | BS.length needle <= 32
    = isInfixOfBrutalBS needle
    | otherwise
    = let !twoWay = indicesTwoWayBS needle
      in not . List.null . twoWay


------ Naive / Brute Force ------

elemIndexFromBS :: Word8 -> Int -> BS.ShortByteString -> Int
{-^ @firstByteIndex b bs@ is the index of the first occurrence of @b@ in @bs@, or, if it does not occur in @bs@, the length of @bs@. -}
elemIndexFromBS b start bs = loop start
  where
    loop !i
        | i < BS.length bs  &&  b /= BS.index bs i
        = loop (i + 1)
        | otherwise
        = i
{-# INLINE elemIndexFromBS #-}

elemBS :: Word8 -> BS.ShortByteString -> Bool
elemBS b bs = elemIndexFromBS b 0 bs /= BS.length bs
{-# NOTINLINE elemBS #-}

elemIndicesBS :: Word8 -> BS.ShortByteString -> [Int]
elemIndicesBS b bs = go 0
  where
    go !i =
        case elemIndexFromBS b i bs of
            i' | i' < BS.length bs -> i' : go (i' + 1)
            _ -> []
{-# NOTINLINE elemIndicesBS #-}

isInfixOfBrutalBS :: BS.ShortByteString -> BS.ShortByteString -> Bool
{-^ O(min 1 (length needle) * length haystack)
Compare all substrings of haystack of length (length needle) to needle; if one is equal, evaluate to True; otherwise False.

isInfixOf "" "" is True because "" is a substring of "".
indices "" "" is [] because "" has no indices.
So isInfixOf needle haystack is not equivalent to not (List.null needle haystack).
-}
isInfixOfBrutalBS needle haystack =
    go (BS.length haystack - l_needle)
    -- Starting with the rightmost, compare all substrings of haystack that are as long as needle to needle. If one matches, evaluate to True; otherwise evaluate to False.
    -- This doesn't need a null needle check. It'll memcmp nothing to nothing once and evaluate to True. Which is plenty efficient for a useless edge case.
  where
    !l_needle = BS.length needle
    go !i
      | i >= 0
      = case compareSlicesBS needle 0 haystack i l_needle of
            EQ -> True
            _  -> go (i - 1)
      | otherwise
      = False
{-# NOTINLINE isInfixOfBrutalBS #-}

indicesBrutalBS :: BS.ShortByteString -> BS.ShortByteString -> [Int]
{-^ the indices at which a substring equal to needle occurs in haystack
Don't use this; instead use 'indices'. Exported only for testing.

WARNING: The result is unspecified for empty needles.
-}
indicesBrutalBS needle haystack = go 0
  where
    !l_needle = BS.length needle
    !end = BS.length haystack - l_needle
    go !i
        | i <= end
        = case compareSlicesBS needle 0 haystack i l_needle of
            EQ -> i : go (i + 1)
            _  -> go (i + 1)
        | otherwise
        = []
{-# NOTINLINE indicesBrutalBS #-}


------ Two-Way (O(n)) ------

indicesTwoWayBS :: BS.ShortByteString -> BS.ShortByteString -> [Int]
indicesTwoWayBS = indicesTwoWayVia eqSlicesBS BS.index BS.length
{-# NOTINLINE indicesTwoWayBS #-}

-- unsafeIndexBS :: BS.ShortByteString -> Int -> Word8
-- unsafeIndexBS (IBS.SBS x) ( I# i ) = W8# ( GHC.indexWord8Array# x i )

{- About Maximal Suffixes, Self-maximal Prefixes, and Periods
 * P is periodic ==> period(MaxSuf(P)) == period(P).
 * P[i..j] is self-maximal, k <= j ==> P[i..k] is self-maximal.

If P is self-maximal, period(P) can be computed by
function Naive-Period(j);
  period := 1;
    for i := 2 to j do
      if P[i] /= p[i - period] then period := i;
return (period);
-}

------ Gianni Franceschini & Torben Hagerup ------
{- int max_suffix_fuzzy( string a, int r0, int n) {
// 0 <= r0 < n
int r = r0, s = r0 + 2, m = 2, f = 1, d = 1, i = 1;
int* M = malloc(sizeOf(int) * (n + 3));
M[2] = 2;
while (s <= n)
  switch (compare(s, s - m, a)) {
  case '<': ++s; m = s - r; M[s - r] = m; break;
  case '=': ++s; M[s - r] = m; break;
  case '>':
    d = (s - r) % m;
    if (d >= f) {
      r = s - d;
      if (d > 1) {
        m = M[d];
      } else {
        m = M[2] = 2; -- WHY?
        s = r + m;
        f = 1;
    } else {
      --m; r = s - m; f = 0;
      for (i = 3; i <= m; i++) { M[i] = min(M[i + 1], i); }
    }
  }
free M;
return (f > 0  &&  r < n  &&  compare(r, r + 1, a) == '<')? r + 1 : r;
}
-}

------ Jewels of Stringology ------

{-
Let pat = uv, where v = MaxSuf(pat).
Search for v with algorithm SpecialCase-MP.
For each occurrence of v at i in text,
  let prev be the previous occurrence of v;
  if i - prev > |u| then
    if u occurs to the left of v then report a match.
    (Occurrence of u is tested naively.)

Algorithm MaxSuffix-Matching
i := 0; j := 0; period := 1; prev := 0;
while i <= |text| - |v| do
  while j < |v|  and  v[j + 1] = text[i + j + 1] do
    j := j + 1;
    if j > period  and  v[j] /= v[j - period]
    then period := j
  if j = |v| then
    if i - prev > |u|  and  u = text[i - |u| + 1..i] then
      report a match at i - |u|;
      prev := i;
  i := i + period;
  if j  >=  2 * period
  then j := j - period
  else j := 0; period := 1;
-}

{-
function MaxSuf-and-Period0(x)
j := 1
repeat  -- (While true)
  (i, period) := Longest-Self-Maximal-Prefix(x[j..n])
  If i = n then return (j, period)
  Else j := j + i - (i mod period)
-}

{-
function MaxSuf-and-Period(x)
s := 1; i := 2; p := 1;
while (i <= n) do
  r := (i - s) mod p
  If (x[i] < x[s + r])
  then i := i + 1; p := i - s;
  else s := i - r; i := s + 1; p := 1;
return (s, p)
{ x[s..n0] = MaxSuf(x); p = period(MaxSuf(x)) }
-}
{-
maxsuf_and_Period_via cmp pat = loop 1 2 1
  where
    x i = BS.index pat (i - 1)
    n = BS.length pat
    loop s i p
        | i <= n
        = let r = (i - s) `mod` p
          in case cmp (x i) (x (s + r)) of
              EQ -> loop s       (i + 1)     p
              LT -> loop s       (i + 1)    (i + 1 - s)
              GT -> loop (i - r) (i + 1 - r) 1
        | otherwise
        = (s, p)
-}
{-
maxSufPerBy cmp pat = loop (-1) 0 1 1
  where
    !n = BS.length pat
    -- s is the (0-based) index of the last byte of the prefix (before the maximal suffix). It never decreases and is always less than i. Using the start of the suffix would make more sense, but this simplifies the math.
    -- i is the forward index into the pattern; it is strictly increasing but can jump forward.
    -- k is the current offset for comparing both the indices into the pattern as long as they are equal, until the period is reached. It starts at 1 rather than 0 to simplify the math.
    loop !s !i !k !p
        | i + k < n
        -- i + k in this is equal to i in the previous algorithm.
        -- k in this is equal to (i - s) `mod` p in the previous algorithm.
        = case BS.index pat (i + k) `cmp` BS.index pat (s + k) of
            EQ                -- s' i'     k'     p'
                | k == p -> loop s (i + p) 1      p  -- This does the modulo.
                | True   -> loop s  i     (k + 1) p
            LT ->           loop s (i + k) 1     (i + k - s)
            GT ->           loop i (i + 1) 1      1  -- More maximal suffix found.
        | otherwise
        = (s + 1, p)

critFact :: BS.ShortByteString -> (Int, Int)
critFact pat
    | s1 >= s2 = sp1
    | otherwise = sp2
  where
    sp1@(s1, _) = maxSufPerBy compare pat
    sp2@(s2, _) = maxSufPerBy (flip compare) pat
-}

{- Simplified Crochemore-Perrin

Two-way_Pattern-Matching
{- period = Period(v) -}
i := |u|; j := 0; prev := 0;
while i <= n - |v| do
  while j < |v| and v[j + 1] = text[i + j + 1] do
    j := j + 1
  if j = |v|  {- MATCH of v -}
  then
    If i - prev  >  |u|   and   u = text[i - |u| + 1 .. i]
    then report a match at i - |u|;
    prev := i; i := i + period; j := |v| - period;
  else  {- MISMATCH of v -}
    i := i + j + 1;
-}
{-
-- This is modified to use 0-based indexing.
two_Way_Pattern_Matching ::
    BS.ShortByteString -> BS.ShortByteString -> [Int]
two_Way_Pattern_Matching pat = \ text ->
  let
    (!l_u, !period) = critFact pat
    !l_v = BS.length pat - l_u  -- length of the maximal suffix
    v i = BS.index pat (l_u + i)
    x = BS.index text
    !n = BS.length text
    loop !i !j !prev
        | i <= n - l_v
        = loop2 j
        | otherwise
        = []
      where
        loop2 !j
            | j < l_v  &&  v (j) == x (i + j)
            = loop2 (j + 1)
            | j == l_v
            = let next = loop (i + period) (l_v - period) i
              in if i - prev  >=  l_u
                 &&  compareSlicesBS pat 0 text (i - l_u) l_u == EQ
                then (i - l_u) : next
                else next
            | otherwise
            = loop (i + j + 1) j prev
  in loop l_u 0 0
-}

{-
The worst case for Two-way_Pattern matching is when pat is self-maximal, because |u| = 0.

The solution to this is to try the both the maximal decomposition by the ordering <= on the alphabet and the ordering >= on the alphabet. (In Haskell this is Ord Word8 and Ord (Down Word8).)

This is called "magic decompositon". For decomposition by <= (u1, v1) and decomposition by >= (u2, v2), if |v1| <= |v2| the magic decomposition is (u1, v1); otherwise it is (u2, v2).
-}
{-
selfmax_period bs = loop 1 1
{-^ period of longest self-maximal prefix -}
  where
    loop i pi
        | i < BS.length bs
        = if BS.index bs i /= BS.index bs (i - pi)
          then loop (i + 1) (pi + 1)
          else loop (i + 1) pi
        | otherwise
        = pi
-}

{-
selfmax_periods bs = loop 1 1
{-^ periods of all self-maximal prefixes -}
  where
    loop i pi
      | i < BS.length bs
      = if BS.index bs i /= BS.index bs (i - pi)
        then (i, pi + 1) : loop (i + 1) (pi + 1)
        else loop (i + 1) pi
      | otherwise
      = []
-}

------

-- rtBGM pat text = loop 0
--   where
--     -- Compute two critical factorizations, one of pat and one of a prefix of pat:
--     -- For factorization pat = uv <> w, use suffix w.
--     -- For factorization pat = u <> vv', use prefix u
--     -- Factor pat into (u, v, v') where
--     -- u = pat[0 .. a - 1], v = pat[a .. b - 1], v' = pat[b .. c - 1]
--     sp1@(s1, p) = maxsuf_and_Period_via compare
--     sp2@(s2, _) = maxSuf_and_Period_via (flip compare)
--     (a, b)
--       | s1 <= s2  = (s1, s2)
--       | otherwise = (s2, s1)
--     m = BS.length pat
--     n = BS.length text
