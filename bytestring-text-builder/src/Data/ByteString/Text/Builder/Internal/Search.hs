{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , MagicHash
           , UnboxedTuples
  #-}

module Data.ByteString.Text.Builder.Internal.Search (
indicesTwoWayVia,
) where

import Data.ByteString.Text.Builder.Internal.Prelude
import qualified GHC.Exts as GHC

{- This module provides basic definitions of search algorithms that would be too cumbersome or error-prone to redefine in ...Text.Strict, ...Text.Short, etc.
-}

------ Adapted from Jewels of Stringology ------

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

maxSuffixPeriodVia ::
    (bs -> Int -> Word8) -> (bs -> Int) ->
    (Word8 -> Word8 -> Ordering) ->
    bs -> (Int, Int)
maxSuffixPeriodVia index length cmp = loop (-1) 0 1 1
  where
        -- s is the (0-based) index of the last byte of the prefix (before the maximal suffix). It never decreases and is always less than i. Using the start of the suffix would make more sense, but this simplifies the math.
        -- i is the forward index into the pattern; it is strictly increasing but can jump forward.
        -- k is the current offset for comparing both the indices into the pattern as long as they are equal, until the period is reached. It starts at 1 rather than 0 to simplify the math.
    loop !s !i !k !p !pat
        | i + k < n
        -- i + k in this is equal to i in the previous algorithm.
        -- k in this is equal to (i - s) `mod` p in the previous algorithm.
        = case index pat (i + k) `cmp` index pat (s + k) of
            EQ                -- s' i'     k'     p'
                | k == p -> loop s (i + p) 1      p          pat  -- This does the modulo.
                | True   -> loop s  i     (k + 1) p          pat
            LT ->           loop s (i + k) 1     (i + k - s) pat
            GT ->           loop i (i + 1) 1      1          pat  -- More maximal suffix found.
        | otherwise
        = (s + 1, p)
      where
        !n = length pat
{-# INLINE maxSuffixPeriodVia #-}

maxSuffixPeriodVia# ::
    ( bs -> Int# -> Word8# ) -> ( bs -> Int# ) ->
    ( Word8# -> Word8# -> Int# ) ->
    bs -> (# Int#, Int# #)
maxSuffixPeriodVia# index length cmp = loop ( -1# ) 0# 1# 1#
  where
    loop !s !i !k !p !pat
        | isTrue# ( i +# k <# n )
        -- i + k in this is equal to i in the previous algorithm.
        -- k in this is equal to (i - s) `mod` p in the previous algorithm.
        = case index pat ( i +# k ) `cmp` index pat ( s +# k ) of
            sign
                | isTrue# ( sign ==# 0# )
                -> if isTrue# ( k ==# p )
                  then loop s ( i +# p )      1#   p pat
                  else loop s   i      ( k +# 1# ) p pat
                | isTrue# ( sign <# 0# )
                -> loop s ( i +# k ) 1# ( i +# k -# s ) pat
                | otherwise
                -> loop i ( i +# 1# ) 1# 1# pat
        | otherwise
        = (# s +# 1#, p #)
      where
        !n = length pat
{-# INLINE maxSuffixPeriodVia# #-}

{-
The worst case for Two-way_Pattern matching is when pat is self-maximal, because |u| = 0.

The solution to this is to try the both the maximal decomposition by the ordering <= on the alphabet and the ordering >= on the alphabet. (In Haskell this is Ord Word8 and Ord (Down Word8).)

This is called "magic decompositon" (critical factorization). For decomposition by <= (u1, v1) and decomposition by >= (u2, v2), if |v1| <= |v2| the magic decomposition is (u1, v1); otherwise it is (u2, v2).
-}

criticalFactorizationVia ::
    (bs -> Int -> Word8) -> (bs -> Int) ->
    bs -> (Int, Int)
criticalFactorizationVia index length = go
  where
    go !pat
        | s1 >= s2 = sp1
        | otherwise = sp2
      where
        sp1@(!s1, !_) = maxSuffixPeriodVia index length compare pat
        sp2@(!s2, !_) = maxSuffixPeriodVia index length (flip compare) pat
{-# INLINE criticalFactorizationVia #-}

criticalFactorizationVia# ::
    ( bs -> Int# -> Word8# ) -> ( bs -> Int# ) ->
    bs -> (# Int#, Int# #)
criticalFactorizationVia# index length = go
  where
    go !pat
        | isTrue# ( s1 >=# s2 ) = (# s1, p1 #)
        | otherwise = (# s2, p2 #)
      where
        (# s1, p1 #) = maxSuffixPeriodVia# index length cmpW8# pat
        (# s2, p2 #) = maxSuffixPeriodVia# index length (\ x y -> cmpW8# y x) pat
        cmpW8# :: Word8# -> Word8# -> Int#
        cmpW8# x y = ( word8ToWord# x `gtWord#` y ) -# ( word8ToWord# x `ltWord#` y )
{-# INLINE criticalFactorizationVia# #-}

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

-- This is modified to use 0-based indexing.
indicesTwoWayVia ::
    (bs -> Int -> bs -> Int -> Int -> Bool) -> (bs -> Int -> Word8) -> (bs -> Int) ->
    bs -> bs -> [Int]
indicesTwoWayVia eqSlices index length = setupTwoWay
  where
    setupTwoWay pat =
        indicesTwoWayLoop#
            (\ bs1 off1 bs2 off2 len ->
              GHC.dataToTag# (eqSlices bs1 (I# off1) bs2 (I# off2) (I# len)))
            index#
            length#
            pat
            l_u
            period
      where
        !(# l_u, period #) = criticalFactorizationVia# index# length# pat
        index# bs i = case index bs (I# i) of W8# w -> w
        {-# INLINE index# #-}
        length# bs = case length bs of I# len -> len
        {-# INLINE length# #-}
  --   go
  -- where
  --   go pat = loop l_u 0 0
  --     where
  --       (!l_u, !period) = criticalFactorizationVia index length pat
  --       !l_v = length pat - l_u  -- length of the maximal suffix
  --       v i = index pat (l_u + i)
  --       loop !i !j !prev !txt
  --           | i <= n - l_v
  --           = loop2 j
  --           | otherwise
  --           = []
  --         where
  --           x = index txt
  --           !n = length txt
  --           loop2 !j
  --               | j < l_v  &&  v j == x (i + j)
  --               = loop2 (j + 1)
  --               | j == l_v
  --               = let next = loop (i + period) (l_v - period) i txt
  --                 in if i - prev  >=  l_u
  --                   &&  eqSlices pat 0 txt (i - l_u) l_u
  --                   then (i - l_u) : next
  --                   else next
  --               | otherwise
  --               = loop (i + j + 1) j prev txt
{-# INLINE indicesTwoWayVia #-}


indicesTwoWayLoop# ::
    ( bs -> Int# -> bs -> Int# -> Int# -> Int# ) ->
    ( bs -> Int# -> Word8# ) ->
    ( bs -> Int# ) ->
    bs -> Int# -> Int# ->
    bs -> [Int]
indicesTwoWayLoop# eqSlices index length = go
  where
    go !pat !l_u !period !txt = loop l_u 0# 0#
      where
        !l_v = length pat -# l_u  -- length of the maximal suffix
        v i = index pat (l_u +# i)
        !n = length txt
        x i = index txt i

        loop !i !j !prev
            | isTrue# ( i <=# n -# l_v )
            = loop2 j
            | otherwise
            = []
          where
            loop2 !j
                | isTrue# ( j <# l_v )
                && isTrue# ( v j `eqWord8#` x ( i +# j ))
                = loop2 ( j +# 1# )
                | isTrue# ( j ==# l_v )
                = let next = loop ( i +# period ) ( l_v -# period ) i
                  in if isTrue# ( i -# prev  >=#  l_u )
                      && isTrue# ( eqSlices pat 0# txt (i -# l_u) l_u )
                      then I# (i -# l_u) : next
                      else next
                | otherwise
                = loop ( i +# j +# 1# ) j prev
{-# INLINE indicesTwoWayLoop# #-}
