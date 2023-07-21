{-# LANGUAGE NoImplicitPrelude
           , ScopedTypeVariables
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , CPP
  #-}

module Data.ByteString.Text.Core.Internal.Utf8 (
isUtf16Surrogate, replaceBadUtf16,
utf8Length, utf8Length#,
utf8LengthByLeader,
foldrCharBytes,
CharBytes (..), charBytes,
char1', char2', char3', char4',
unChar1, unChar2#, unChar3#, unChar4#,
) where

import Data.ByteString.Text.Core.Internal.Prelude
import qualified GHC.Exts as GHC

isUtf16Surrogate :: Char -> Bool
{-^
Surrogates are all code points in the ranges xD800 to xDFFF.

Unicode mistakenly tied code points to 16-bit representation and created meaningless "surrogate" code points that, when paired in the correct order, map in UTF-16 to a valid code point outside the Basic Multilingual Plane.

A 'Char' can be a single ("unpaired ") surrogate. UTF-8 cannot encode these error states, so UTF-16 surrogeates must be mapped to the Unicode replacement character (or an error must be thrown).
-}
isUtf16Surrogate c = '\xD800' <= c  &&  c <= '\xDFFF'
{-# INLINE isUtf16Surrogate #-}

replaceBadUtf16 :: Char -> Char
replaceBadUtf16 c
    | isUtf16Surrogate c = '\xFFFD'
    | otherwise = c
{-# INLINABLE replaceBadUtf16 #-}


utf8LengthByLeader :: Word8 -> Int
{-^ @utf8LengthByLeader w@ is
* 0 if w is a follower
* 1 if w is ASCII
* 2 if w is a leader of 1
* 3 if w is a leader of 2
* 4 if w is a leader of 3
* 5 to 8 for invalid bytes
-}
utf8LengthByLeader ( W8# w8# ) = GHC.I# ( utf8LengthByLeader# w8# )
{-# INLINE utf8LengthByLeader #-}

utf8LengthByLeader# :: Word8# -> Int#
utf8LengthByLeader# w8# =
    case countLeadingZeros (complement ( W8# w8# )) of
        -- This makes it 1 for ASCII and 0 for a follower; otherwise it would be the opposite:
        GHC.I# n# -> xorI# n# ( n# <=# 0# )
{-# INLINE utf8LengthByLeader# #-}

utf8Length :: Char -> Int
utf8Length ( GHC.C# c# ) = GHC.I# ( utf8Length# c# )
{-# INLINE utf8Length #-}

utf8Length# :: Char# -> Int#
utf8Length# c =
       1#
    +# geChar# c (chr# 0x80#)
    +# geChar# c (chr# 0x800#)
    +# geChar# c (chr# 0x10000#)
{-# INLINE utf8Length# #-}


data CharBytes
    = CharBytes1 !Word8
    | CharBytes2 !Word8 !Word8
    | CharBytes3 !Word8 !Word8 !Word8
    | CharBytes4 !Word8 !Word8 !Word8 !Word8

charBytes :: Char -> CharBytes
charBytes c@( GHC.C# c# ) = case utf8Length# c# of
    1# -> CharBytes1 (unChar1 c)
    2# -> case unChar2# c of (# w0, w1 #) -> CharBytes2 w0 w1
    3# -> case unChar3# c of (# w0, w1, w2 #) -> CharBytes3 w0 w1 w2
    _  -> case unChar4# c of (# w0, w1, w2, w3 #) -> CharBytes4 w0 w1 w2 w3
{-# INLINABLE charBytes #-}

foldrCharBytes :: (Word8 -> b -> b) -> b -> Char -> b
foldrCharBytes f z c@( GHC.C# c# ) = case utf8Length# c# of
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
