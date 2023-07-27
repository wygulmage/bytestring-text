{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
           , BangPatterns
  #-}

{-# OPTIONS_HADDOCK not-home
  #-}

{-| This is an internal module. It is not considered part of the API of bytestring-text-builder. Definitions may be added, CHANGED, or REMOVED without warning. If you depend on this module, make sure to set exact upper and lower bounds on the bytestring-text-builder version.

This module provides basic UTF-8 functions for all the other @Data.ByteString.Text@ modules.
-}

module Data.ByteString.Text.Builder.Internal.Utf8 (
isUtf16Surrogate, replaceBadUtf16,
-- Helpers for consuming UTF-8
foldrIndexLen,
foldr'IndexLen,
foldlIndexLen,
foldl'IndexLen,
unsafeIndexNextVia,
unsafeIndexBackVia,
) where

import Data.ByteString.Text.Builder.Internal.Prelude

replaceBadUtf16 :: Char -> Char
{-^ @replaceBadUtf16 c@ is @'\\xFFFD'@ if c is a UTF-16 surrogate, otherwise it is @c@.

Surrogates are all code points in the ranges xD800 to xDFFF.

Unicode mistakenly tied code points to 16-bit representation and created meaningless "surrogate" code points that, when paired in the correct order, map in UTF-16 to a valid code point outside the Basic Multilingual Plane.

A 'Char' can be a single ("unpaired ") surrogate. UTF-8 cannot encode these error states, so UTF-16 surrogeates must be mapped to the Unicode replacement character (or an error must be thrown).
-}
replaceBadUtf16 c
    | isUtf16Surrogate c = '\xFFFD'
    | otherwise = c
{-# INLINABLE replaceBadUtf16 #-}


isUtf16Surrogate :: Char -> Bool
{-^
Surrogates are all code points in the ranges xD800 to xDFFF.

Unicode mistakenly tied code points to 16-bit representation and created meaningless "surrogate" code points that, when paired in the correct order, map in UTF-16 to a valid code point outside the Basic Multilingual Plane.

A 'Char' can be a single ("unpaired ") surrogate. UTF-8 cannot encode these error states, so UTF-16 surrogeates must be mapped to the Unicode replacement character (or an error must be thrown).
-}
isUtf16Surrogate c = '\xD800' <= c  &&  c <= '\xDFFF'
{-# INLINE isUtf16Surrogate #-}

foldrIndexLen ::
    (bytes -> Int -> Word8) -> (bytes -> Int) ->
    (Char -> a -> a) -> a -> bytes -> a
foldrIndexLen index length = \ f z ->
  let
    foldr_go !i bytes
        | isTrue# (i <# length')
        = case unsafeIndexNextVia# index' i of
            (# c, i' #) -> c `f` foldr_go i' bytes
        | otherwise
        = z
      where
        index' i = index bytes ( I# i )
        !( I# length' ) = length bytes
  in foldr_go 0#

{-# INLINE foldrIndexLen #-}

foldl'IndexLen ::
    (bytes -> Int -> Word8) -> (bytes -> Int) ->
    (b -> Char -> b) -> b -> bytes -> b
foldl'IndexLen index length = \ f z0 ->
  let
    foldl'_loop !i z bytes
        | isTrue# ( i <# length' )
        = case unsafeIndexNextVia# index' i of
            (# c, i' #) ->
                let !z' = f z c in foldl'_loop i' z' bytes
        | otherwise
        = z
      where
        !( I# length' ) = length bytes
        index' i = index bytes ( I# i )
  in foldl'_loop 0# z0
{-# INLINE foldl'IndexLen #-}

foldlIndexLen ::
    (bytes -> Int -> Word8) -> (bytes -> Int) ->
    (a -> Char -> a) -> a -> bytes -> a
foldlIndexLen index length = \ f z -> \ bytes ->
  let
    !( I# length' ) = length bytes
    index' i = index bytes ( I# i )
    foldl_go !i
        | isTrue# ( i >=# 0# )
        = case unsafeIndexBackVia# index' i of
            (# c, i' #) ->
                foldl_go i' `f` c
        | otherwise
        = z
  in foldl_go ( length' -# 1# )
{-# INLINE foldlIndexLen #-}

foldr'IndexLen ::
    (bytes -> Int -> Word8) -> (bytes -> Int) ->
    (Char -> a -> a) -> a -> bytes -> a
foldr'IndexLen index length = \ f z0 -> \ bytes ->
  let
    index' i = index bytes ( I# i )
    !( I# length' ) = length bytes
    foldr'_loop !i z
        | isTrue# (i >=# 0#)
        = case unsafeIndexBackVia# index' i of
            (# c, i' #) ->
              let !z' = f c z in foldr'_loop i' z'
        | otherwise
        = z
  in foldr'_loop ( length' -# 1# ) z0
{-# INLINE foldr'IndexLen #-}

unsafeIndexNextVia :: ( Int -> Word8 ) -> Int -> (# Char, Int #)
unsafeIndexNextVia index ( I# i ) =
    case unsafeIndexNextVia# (\ i' -> index ( I# i' )) i of
        (# c, i'' #) -> (# c, I# i'' #)
{-# INLINE unsafeIndexNextVia #-}

unsafeIndexNextVia# :: ( Int# -> Word8 ) -> Int# -> (# Char, Int# #)
unsafeIndexNextVia# index i =
    case unsafeIndexLenVia# index i of (# c, off #) -> (# c, off +# i #)
{-# INLINE unsafeIndexNextVia# #-}

unsafeIndexLenVia# :: ( Int# -> Word8 ) -> Int# -> (# Char, Int# #)
unsafeIndexLenVia# index i0 = (# uncheckedChr' c, l #)
  where
    -- For now this is a thunk to make `uncons` lazier, but could force it or produce a Char# rather than a Char.
    c = case l of
        1# -> char1' b0
        2# -> char2' b0 b1
        3# -> char3' b0 b1 b2
        _ -> char4' b0 b1 b2 b3
    !(I# l) = utf8LengthByLeader b0
    !b0 = index i0
    b1 = index ( i0 +# 1# )
    b2 = index ( i0 +# 2# )
    b3 = index ( i0 +# 3# )
{-# INLINE unsafeIndexLenVia# #-}

unsafeIndexBackVia :: ( Int -> Word8 ) -> Int -> (# Char, Int #)
unsafeIndexBackVia index ( I# i ) =
    case unsafeIndexBackVia# (\ i' -> index ( I# i' )) i of
        (# c, i'' #) -> (# c, I# i'' #)
{-# INLINE unsafeIndexBackVia #-}

unsafeIndexBackVia# :: ( Int# -> Word8 ) -> Int# -> (# Char, Int# #)
unsafeIndexBackVia# index i =
    case unsafeIndexLenEndVia# index i of (# c, off #) -> (# c, off +# i #)
{-# INLINE unsafeIndexBackVia# #-}

unsafeIndexLenEndVia# :: ( Int# -> Word8 ) -> Int# -> (# Char, Int# #)
{-^ O(1)
Given the byte index of the end of a UTF-8-encoded 'Char' and an indexing function into an input, return the whole 'Char' and its encoded length.

This is unsafe because it does not check if its input is empty or UTF-8.
-}
unsafeIndexLenEndVia# index end =
    (# uncheckedChr' c, diff #)
  where
    !(# diff, c #)
        | w0 <= 0x7F = (# 0#, char1' w0 #)
        | w1 >= 0xC0 = (# 1#, char2' w1 w0 #)
        | w2 >= 0xC0 = (# 2#, char3' w2 w1 w0 #)
        | otherwise  = (# 3#, char4' w3 w2 w1 w0 #)
    !w0 = index end
    w1 = index ( end -# 1# )
    w2 = index ( end -# 2# )
    w3 = index ( end -# 3# )
{-# INLINE unsafeIndexLenEndVia# #-}



utf8LengthByLeader :: Word8 -> Int
{-^ @utf8LengthByLeader w@ is
* 0 if w is a follower
* 1 if w is ASCII
* 2 if w is a leader of 1
* 3 if w is a leader of 2
* 4 if w is a leader of 3
* 5 to 8 for invalid bytes
-}
utf8LengthByLeader ( W8# w8# ) = I# ( utf8LengthByLeader# w8# )
{-# INLINE utf8LengthByLeader #-}

utf8LengthByLeader# :: Word8# -> Int#
utf8LengthByLeader# w8# =
    case countLeadingZeros (complement ( W8# w8# )) of
        -- This makes it 1 for ASCII and 0 for a follower; otherwise it would be the opposite:
        I# n# -> xorI# n# ( n# <=# 0# )
{-# INLINE utf8LengthByLeader# #-}

utf8Length :: Char -> Int
utf8Length ( C# c# ) = I# ( utf8Length# c# )
{-# INLINE utf8Length #-}

utf8Length# :: Char# -> Int#
utf8Length# c =
       1#
    +# geChar# c (chr# 0x80#)
    +# geChar# c (chr# 0x800#)
    +# geChar# c (chr# 0x10000#)
{-# INLINE utf8Length# #-}

foldrCharBytes :: (Word8 -> b -> b) -> b -> Char -> b
foldrCharBytes f z c@( C# c# ) = case utf8Length# c# of
    1# -> case unChar1 c of
        !w0 -> f w0 z
    2# -> case unChar2# c of
        (# w0, w1 #) -> f w0 (f w1 z)
    3# -> case unChar3# c of
        (# w0, w1, w2 #) -> f w0 (f w1 (f w2 z))
    _ -> case unChar4# c of
        (# w0, w1, w2, w3 #) -> f w0 (f w1 (f w2 (f w3 z)))
{-# INLINE foldrCharBytes #-}

unChar1 :: Char -> Word8
unChar1 = fromIntegral . ord

unChar2# :: Char -> (# Word8, Word8 #)
unChar2# c = (# w0, w1 #)
  where
    i = ord c
    !w0 = 0xC0 .|. fromIntegral (unsafeShiftR i 6)
    !w1 = markTail $ fromIntegral i

unChar3# :: Char -> (# Word8, Word8, Word8 #)
unChar3# c = (# w0, w1, w2 #)
  where
    i = ord c
    !w0 = 0xE0 .|. fromIntegral (unsafeShiftR i 12)
    !w1 = markTail $ fromIntegral $ unsafeShiftR i 6
    !w2 = markTail $ fromIntegral i

unChar4# :: Char -> (# Word8, Word8, Word8, Word8 #)
unChar4# c = (# w0, w1, w2, w3 #)
  where
    i = ord c
    !w0 = 0xF0 .|. fromIntegral (unsafeShiftR i 18)
    !w1 = markTail $ fromIntegral $ unsafeShiftR i 12
    !w2 = markTail $ fromIntegral $ unsafeShiftR i 6
    !w3 = markTail $ fromIntegral i

markTail :: Word8 -> Word8
markTail b = 0x80 .|. unmarkTail b
{-# INLINE markTail #-}

unmarkTail :: Word8 -> Word8
unmarkTail = (.&.) 0x3F
{-# INLINE unmarkTail #-}

char1' :: Word8 -> Word
char1' = fromIntegral
{-# INLINE char1' #-}

char2' :: Word8 -> Word8 -> Word
char2' w0 w1 = w0' .|. w1'
  where
    !w0' = unsafeShiftL (fromIntegral (0x3F .&. w0)) 6
    !w1' = fromIntegral (unmarkTail w1)

char3' :: Word8 -> Word8 -> Word8 -> Word
char3' w0 w1 w2 = w0' .|. w1' .|. w2'
  where
    !w0' = unsafeShiftL (fromIntegral (0xF .&. w0)) 12
    !w1' = unsafeShiftL (fromIntegral (unmarkTail w1)) 6
    !w2' = fromIntegral (unmarkTail w2)

char4' :: Word8 -> Word8 -> Word8 -> Word8 -> Word
char4' w0 w1 w2 w3 =
    (w0' .|. w1') .|. (w2' .|. w3')
  where
    !w0' = unsafeShiftL (fromIntegral (0xF .&. w0)) 18
    !w1' = unsafeShiftL (fromIntegral (unmarkTail w1)) 12
    !w2' = unsafeShiftL (fromIntegral (unmarkTail w2)) 6
    !w3' = fromIntegral (unmarkTail w3)

uncheckedChr' :: Word -> Char
uncheckedChr' ( W# w ) = C# (chr'# w )
{-# INLINE uncheckedChr' #-}
