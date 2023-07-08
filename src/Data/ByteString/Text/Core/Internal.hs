{-# LANGUAGE NoImplicitPrelude
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , TypeFamilies
  #-}

{- This is an internal module of bytestring-text, and is not subject to the usual package versioning policy for API changes. By using this API you can violate invariants that are assumed in the 'Data.ByteString.Text' API, and even violate memory safety! Have fun!
-}

module Data.ByteString.Text.Core.Internal (
Text (..),
-- * Construct:
pack,
append, concat, empty,
-- * Consume:
unpack,
foldr,
uncons, unsnoc,
-- * Summarize:
null,
measureOff,
lengthWord8,
-- * Unsafe operations:
unsafeHead, unsafeTail,
-- * Unsafe Byte-based Operations:
takeWord8,
dropWord8,
-- * Builder:
Builder (..),
toText, toTextWith,
charUtf8, stringUtf8, fromText,
-- * Helpers:
CharBytes(..), charBytes,
foldrCharBytes,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import Control.DeepSeq (NFData)
import GHC.Base hiding (empty, foldr)
import qualified GHC.Exts as GHC
import GHC.Exts
    ( IsString (..)
    , IsList (..)
    )
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import qualified GHC.Word as GHC
import GHC.Word (Word8)
import Data.Bits
    ((.&.), (.|.), complement, countLeadingZeros, shift)
import Data.Char
-- import Data.Coerce (coerce) -- provided by GHC.Base
import Foreign.Storable (sizeOf)
import Text.Read
    ( Read (readPrec, readList, readListPrec)
    , readListDefault, readListPrecDefault
    )
import Text.Show


newtype Text = UnsafeFromByteString BS.ByteString
  deriving newtype
    ( Ord, Eq
    , Monoid, Semigroup
    , NFData
    )

-- Concrete versions of Monoid methods:

empty :: Text
{-^ "" -}
empty = mempty
{-# INLINE empty #-}

append :: Text -> Text -> Text
{-^ O(n + m) -}
append = (<>)
{-# INLINE append #-}

concat :: [Text] -> Text
concat = mconcat
{-# INLINE concat #-}

-- Instances:

instance Read Text where
    readPrec = fmap pack readPrec
    readList = readListDefault
    readListPrec = readListPrecDefault

instance Show Text where
    showsPrec _ = showList . unpack

instance IsString Text where
    fromString = pack
    {-# INLINE fromString #-}

instance IsList Text where
    type Item Text = Char
    toList = unpack
    {-# INLINE toList #-}
    fromList = pack
    {-# INLINE fromList #-}
    fromListN = packN
    {-# INLINE fromListN #-}

pack :: [Char] -> Text
pack = toText . stringUtf8
{-# INLINE pack #-}

packN :: Int -> [Char] -> Text
packN n cs = toTextWith n' (stringUtf8 cs)
  where
    n4 = n * 4 -- maximum number of bytes in a packed string of n Char. Should this instead optimistically try n * 3?
    !n'
      | Builder.defaultChunkSize <= n4 = defaultChunkSize
      | otherwise = n4
{-# INLINE packN #-}

unpack :: Text -> [Char]
unpack cs = GHC.build (\ c n -> foldr c n cs)
{-# INLINE unpack #-}

-- foldr is needed for unpack.
foldr :: (Char -> b -> b) -> b -> Text -> b
{-^ O(n) (lazy) -}
foldr f z = loop
  where
    loop cs =
        case uncons cs of
            Nothing -> z
            Just (c, cs') -> c `f` loop cs'
{-# INLINABLE [0] foldr #-}

-- uncons is needed for foldr.
uncons :: Text -> Maybe (Char, Text)
{-^ O(1) If the 'Text' is null, 'Nothing'; otherwise 'Just' the first 'Char' and the rest of the 'Text'. -}
uncons cs
    | null cs = Nothing
    | otherwise = Just (case uncons# cs of (# c, cs' #) -> (c, cs'))
{-# INLINE uncons #-}

-- null is needed for uncons.
null :: Text -> Bool
{-^ O(1) Is the text empty? -}
null = coerce BS.null
{-# INLINE null #-}

unsafeHead :: Text -> Char
{-^ O(1) Take the first 'Char' of a 'Text' without checking whether the 'Text' is 'null'. See also: 'head', 'uncons'.

WARNING: The behavior of @unsafeHead txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
-}
unsafeHead cs = case unsafeHeadLen# cs of (# c, _ #) -> c
{-# INLINABLE unsafeHead #-}

uncons# :: Text -> (# Char, Text #)
{-^ O(1) The first 'Char' and the rest of the 'Text'. See also: 'uncons'.

WARNING: The behavior of @uncons# txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
-}
uncons# cs = case unsafeHeadLen# cs of
    (# c, l #) -> let !cs' = dropWord8 (I# l) cs in (# c, cs' #)
{-# INLINABLE uncons# #-}

unsafeHeadLen# :: Text -> (# Char, Int# #)
unsafeHeadLen# (UnsafeFromByteString bs) = (# c, l #)
  where
    c = case l of
        1# -> char1 b0
        2# -> char2 b0 b1
        3# -> char3 b0 b1 b2
        _ -> char4 b0 b1 b2 b3
    !(I# l) = utf8LengthByLeader b0
    !b0 = BS.unsafeHead bs
    b1 = BS.unsafeIndex bs 1
    b2 = BS.unsafeIndex bs 2
    b3 = BS.unsafeIndex bs 3
{-# INLINE unsafeHeadLen# #-}

unsnoc :: Text -> Maybe (Text, Char)
unsnoc cs
    | null cs   = Nothing
    | otherwise = Just (case unsnoc# cs of (# cs', c #) -> (cs', c))
{-# INLINE unsnoc #-}

unsnoc# :: Text -> (# Text, Char #)
{-^ O(1) The last 'Char' and the rest of the 'Text'. See also: 'unsnoc'.

WARNING: The behavior of @unsnoc# txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
-}
unsnoc# (UnsafeFromByteString bs) =
    (# bs', c #)
  where
    !bs' = UnsafeFromByteString (BS.unsafeTake length' bs)  -- forced even though it may not be used.
    !length' = end - diff
    !(diff, c)
        | w0 <= 0x7F = (0, char1 w0)
        | w1 >= 0xC0 = (1, char2 w1 w0)
        | w2 >= 0xC0 = (2, char3 w2 w1 w0)
        | otherwise  = (3, char4 w3 w2 w1 w0)
    !end = BS.length bs - 1  -- index of the last byte of the ByteString
    !w0 = BS.unsafeIndex bs end -- BS.last bs
    w1 = BS.unsafeIndex bs (end - 1)
    w2 = BS.unsafeIndex bs (end - 2)
    w3 = BS.unsafeIndex bs (end - 3)
{-# INLINABLE unsnoc# #-}


unsafeTail :: Text -> Text
{-^ O(1) Drop the first 'Char' from the 'Text'. See also: 'tail', 'uncons'.

WARNING: The behavior of @unsafeTail txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
-}
unsafeTail (UnsafeFromByteString bs) = UnsafeFromByteString bs'
  where
    bs' = BS.unsafeDrop (utf8LengthByLeader (BS.unsafeHead bs)) bs
{-# INLINABLE unsafeTail #-}

-- Honestly I'm not sure why measureOff is part of the primary Text API or even what it's supposed to do. This is my best guess from the Data.Text documentation.
measureOff :: Int -> Text -> Int
{-^ @measureOff n cs@
* if @n@ is 0 or @cs@ is 'empty', is zero;
* otherwise if @cs@ contains at least @n@ 'Char's, is the number of bytes used to encode the first @n@ 'Char's in @cs@;
* otherwise it is @'length' cs@, negated.
-}
measureOff n (UnsafeFromByteString bs) = measureOff_loop 0 0
  where
    measureOff_loop !charCount !byteCount
        | charCount >= n  -- n Chars have successfully been counted.
        = byteCount
        | byteCount >= BS.length bs  -- The end of the Text was reached without counting enough Chars.
        = negate charCount
        | otherwise
        = let charWidth = utf8LengthByLeader (BS.unsafeIndex bs byteCount)
          in measureOff_loop (charCount + 1) (byteCount + charWidth)
-- TODO: Check if it's more efficient to loop over every byte and count only leaders.

{-
measureOffEnd :: Int -> Text -> Int
{-^ @measureOff n cs@
* if @n@ is 0 or @cs@ is 'empty', is zero;
* otherwise if @cs@ contains at least @n@ 'Char's, is the number of bytes used to encode the last @n@ 'Char's in @cs@;
* otherwise it is @'length' cs@, negated.
-}
measureOffEnd n (UnsafeFromByteString bs) =
    measureOffEnd_loop 0 end
  where
    !end = BS.length bs - 1
    measureOffEnd_loop !charCount !i
        | charCount >= n
        = end - i
        | i < 0
        = negate charCount
        | otherwise
        = let
            charWidth
                | w0 <= 0x7F = 1
                | w1 >= 0xC0 = 2
                | w2 >= 0xE0 = 3
                | otherwise  = 4
            !w0 = BS.unsafeIndex bs i
            w1 = BS.unsafeIndex bs (i - 1)
            w2 = BS.unsafeIndex bs (i - 2)
          in measureOffEnd_loop (charCount + 1) (i - charWidth)
-}

--- Operations on the underlying 'ByteString':

lengthWord8 :: Text -> Int
{-^ O(1) The length of the 'Text' in UTF-8 code units ('Word8').
This operation is perfectly safe and is provided by the @Unsafe@ module for historical reasons.
-}
lengthWord8 = coerce BS.length
{-# INLINE lengthWord8 #-}

takeWord8 :: Int -> Text -> Text
{-^ O(1) @takeWord8 n txt@ is a 'Text' consisting of the first @n@ UTF-8 code units ('Word8') of @txt@.

WARNING:
 * This will silently produce invalid 'Text' values if it takes only part of a (UTF-8 encoded) 'Char'.
* The behavior is unspecified if @n > 'lengthWord8' txt@. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
You must ensure that @n <= 'lengthWord8' txt@ and that the last code unit taken is the end of a complete (UTF-8 encoded) 'Char'.

@takeWord8@ is used to define 'take' and 'span'.
-}
takeWord8 = coerce BS.unsafeTake
{-# INLINE takeWord8 #-}

dropWord8 :: Int -> Text -> Text
{-^ O(1) @dropWord8 n txt@ is a 'Text' consisting of all except the first @n@ UTF-8 code units ('Word8') of @txt@. If @'lengthWord8' txt < n@, it is 'empty'.

WARNING:
 * This will silently produce invalid 'Text' values if it drops only part of a UTF-8 encoded 'Char'.
* The behavior is unspecified if @n > 'lengthWord8' txt@. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
You must ensure that @n <= 'lengthWord8' txt@ and that the last code unit dropped is the end of a complete (UTF-8 encoded) 'Char'.

@dropWord8@ is used to define 'takeEnd' and 'takeWhileEnd'.
-}
dropWord8 = coerce BS.unsafeDrop
{-# INLINE dropWord8 #-}

------ Builder ------

newtype Builder = UnsafeFromBuilder Builder.Builder
  deriving newtype
    (Monoid, Semigroup)

instance IsString Builder where
    fromString = stringUtf8
    {-# INLINE fromString #-}

instance Show Builder where
    showsPrec _ = showList . unpack . toText -- FIXME: be lazier!

instance Eq Builder where
    UnsafeFromBuilder k1 == UnsafeFromBuilder k2 =
        Builder.toLazyByteString k1 == Builder.toLazyByteString k2

instance Ord Builder where
    UnsafeFromBuilder k1 `compare` UnsafeFromBuilder k2 =
        Builder.toLazyByteString k1 `compare` Builder.toLazyByteString k2

-- Can't name this 'singleton', so we'll keep ByteString's name and rename it when we export it.
charUtf8 :: Char -> Builder
charUtf8 = coerce Builder.charUtf8
{-# INLINE charUtf8 #-}

fromText :: Text -> Builder
fromText = coerce Builder.byteString
{-# INLINE fromText #-}

-- Can't name this 'fromString', so we'll keep ByteString's name until we export it.
stringUtf8 :: [Char] -> Builder
stringUtf8 = coerce Builder.stringUtf8
{-# INLINE stringUtf8 #-}

-- We don't have lazy Text yet, so make do with these:
toText :: Builder -> Text
{-^ O(n) Convert 'Builder' to 'Text'. -}
toText = toTextWith smallChunkSize
{-# INLINE toText #-}

toTextWith :: Int -> Builder -> Text
{-^ O(n) Convert 'Builder' to 'Text'. The @Int@ is the first buffer size, and should be your best estimate of the size of the result 'Text'.
-}
toTextWith sizeHint (UnsafeFromBuilder k) =
    UnsafeFromByteString (LBS.toStrict (Builder.toLazyByteStringWith
        (Builder.safeStrategy sizeHint defaultChunkSize)
        LBS.empty
        k))
{-# INLINE toTextWith #-}

defaultChunkSize :: Int
{-^ Default chunk size in bytes. Currently 16 kibibites minus 'chunkOverhead'. -}
defaultChunkSize = 16 * 1024  -  chunkOverhead

smallChunkSize :: Int
{-^ Small chunk size in bytes. Currently 128 bytes minus GHC's memory 'chunkOverhead'. -}
smallChunkSize = 128 - chunkOverhead

chunkOverhead :: Int
{-^ GHC's memory management overhead (as of writing); for a 64-bit system this is 16 bytes. -}
chunkOverhead = 2 * sizeOf (undefined :: Int)

------ Helpers ------

utf8LengthByLeader :: GHC.Word8 -> Int
utf8LengthByLeader w8 = GHC.I# (n `GHC.xorI#` (n <=# 0#))
  where
    !(GHC.I# n) = countLeadingZeros (complement w8)
{-# INLINE utf8LengthByLeader #-}

utf8Length :: Char -> Int
utf8Length ( GHC.C# c# ) = GHC.I# ( utf8Length# c# )

utf8Length# :: GHC.Char# -> GHC.Int#
utf8Length# c =
       1#
    +# GHC.geChar# c (GHC.chr# 0x80#)
    +# (GHC.geChar# c (GHC.chr# 0x800#)
    +# GHC.geChar# c (GHC.chr# 0x10000#))
{-# INLINE utf8Length# #-}


data CharBytes
    = CharBytes1 !Word8
    | CharBytes2 !Word8 !Word8
    | CharBytes3 !Word8 !Word8 !Word8
    | CharBytes4 !Word8 !Word8 !Word8 !Word8

charBytes :: Char -> CharBytes
charBytes c@( C# c# ) = case utf8Length# c# of
    1# -> CharBytes1 (unChar1 c)
    2# -> case unChar2# c of (# w0, w1 #) -> CharBytes2 w0 w1
    3# -> case unChar3# c of (# w0, w1, w2 #) -> CharBytes3 w0 w1 w2
    _ -> case unChar4# c of (# w0, w1, w2, w3 #) -> CharBytes4 w0 w1 w2 w3
{-# INLINABLE charBytes #-}

-- foldrCharBytes :: (Word8 -> b -> b) -> b -> CharBytes -> b
-- foldrCharBytes f z = go
--   where
--     go (CharBytes1 w0) = f w0 z
--     go (CharBytes2 w0 w1) = f w0 (f w1 z)
--     go (CharBytes3 w0 w1 w2) = f w0 (f w1 (f w2 z))
--     go (CharBytes4 w0 w1 w2 w3) = f w0 (f w1 (f w2 (f w3 z)))
-- {-# INLINE foldrCharBytes #-}

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
    !w0 = 0xC0 .|. fromIntegral (shift i (-6))
    !w1 = markTail $ fromIntegral i

unChar3# :: Char -> (# Word8, Word8, Word8 #)
unChar3# c = (# w0, w1, w2 #)
  where
    i = ord c
    !w0 = 0xE0 .|. fromIntegral (shift i (-12))
    !w1 = markTail $ fromIntegral $ shift i (-6)
    !w2 = markTail $ fromIntegral i

unChar4# :: Char -> (# Word8, Word8, Word8, Word8 #)
unChar4# c = (# w0, w1, w2, w3 #)
  where
    i = ord c
    !w0 = 0xF0 .|. fromIntegral (shift i (-18))
    !w1 = markTail $ fromIntegral $ shift i (-12)
    !w2 = markTail $ fromIntegral $ shift i (-6)
    !w3 = markTail $ fromIntegral i

markTail :: Word8 -> Word8
markTail b = 0x80 .|. unmarkTail b
{-# INLINE markTail #-}

unmarkTail :: Word8 -> Word8
unmarkTail = (.&.) 0x3F
{-# INLINE unmarkTail #-}

char1 :: Word8 -> Char
char1 w0 = chr (fromIntegral w0)
{-# INLINE char1 #-}

char2 :: Word8 -> Word8 -> Char
char2 w0 w1 = chr $ w0' .|. w1'
  where
    !w0' = shift (fromIntegral (0x3F .&. w0)) 6
    !w1' = fromIntegral (unmarkTail w1)

char3 :: Word8 -> Word8 -> Word8 -> Char
char3 w0 w1 w2 = chr $ w0' .|. w1' .|. w2'
  where
    !w0' = shift (fromIntegral (0xF .&. w0)) 12
    !w1' = shift (fromIntegral (unmarkTail w1)) 6
    !w2' = fromIntegral (unmarkTail w2)

char4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
char4 w0 w1 w2 w3 =
    chr $ (w0' .|. w1') .|. (w2' .|. w3')
  where
    !w0' = shift (fromIntegral (0xF .&. w0)) 18
    !w1' = shift (fromIntegral (unmarkTail w1)) 12
    !w2' = shift (fromIntegral (unmarkTail w2)) 6
    !w3' = fromIntegral (unmarkTail w3)

{-
wordToChar :: GHC.Word -> Char
wordToChar x = chr (fromIntegral x)
{-# INLINE wordToChar #-}

charToWord :: Char -> GHC.Word
charToWord x = fromIntegral (ord x)
{-# INLINE charToWord #-}

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral
{-# INLINE intToWord8 #-}

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
{-# INLINE word8ToInt #-}
-}
