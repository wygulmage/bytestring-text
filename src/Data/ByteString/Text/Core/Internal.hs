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

{- This is an internal module of bytestring-text, and is not subject to the usual package versioning policy for API changes. By using this API you can violate invariants that are assumed in the 'Data.ByteString.Text' API, and even violate memory safety! Have fun!
-}

module Data.ByteString.Text.Core.Internal (
Text (..),
-- * Construct:
pack,
append, concat, empty,
decodeUtf8,
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
isValidUtf8,
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
import GHC.Word
import Data.Bits
    ( (.&.), (.|.), complement, countLeadingZeros
    , shift, unsafeShiftL, unsafeShiftR,
    )
import Data.Char
-- import Data.Coerce (coerce) -- provided by GHC.Base
import Foreign.Storable (sizeOf)
import Text.Read
    ( Read (readPrec, readList, readListPrec)
    , readListDefault, readListPrecDefault
    )
import Text.Show

-- Doctests don't work in this module because it uses UnboxedTuples.

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
{-^ -}
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
{-^ O(1) If the 'Text' is null, 'Nothing'; otherwise 'Just' the first 'Char' and the rest of the 'Text'.

@\ cs -> uncons cs == Just (unsafeHead cs, unsafeTail cs) ||  uncons cs == Nothing@

@uncons (fromString "儵魚出遊從容") = Just ('儵', "魚出遊從容")@
-}
uncons cs
    | null cs = Nothing
    | otherwise = Just (case uncons# cs of (# c, cs' #) -> (c, cs'))
{-# INLINE uncons #-}

-- null is needed for uncons.
null :: Text -> Bool
{-^ O(1) Is the text empty?

@\ cs -> null cs  ==  (length cs == 0)@

@null (fromString "​") == False@

@null (fromString "") == True@
-}
null = coerce BS.null
{-# INLINE null #-}

unsafeHead :: Text -> Char
{-^ O(1) Take the first 'Char' of a 'Text' without checking whether the 'Text' is 'null'. See also: 'head', 'uncons'.

WARNING: The behavior of @unsafeHead txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)

@unsafeHead (fromString "змей") == 'з'@
-}
-- TODO: Was trying to use a Malayalam example here, but it kept crashing emacs or Windows terminal. unsafeHead (paanvu) = pa
unsafeHead cs = case unsafeHeadLen# cs of (# c, _ #) -> c
{-# INLINABLE unsafeHead #-}

uncons# :: Text -> (# Char, Text #)
{-^ O(1) The first 'Char' and the rest of the 'Text'. See also: 'uncons'.

WARNING: The behavior of @uncons# txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
-}
uncons# cs =
    case unsafeHeadLen# cs of
        (# c, l #) ->
          let !cs' = dropWord8 (I# l) cs
          in (# c, cs' #)
{-# INLINE uncons# #-}

unsafeHeadLen# :: Text -> (# Char, Int# #)
{-^ O(1)
The first 'Char' of the 'Text' and the number of bytes needed to encode that 'Char' in UTF-8.

WARNING: @unsafeHeadLen# txt@ is unspecified if @null txt@ is 'True'.
-}
unsafeHeadLen# (UnsafeFromByteString bs) = (# c, l #)
  where
    -- For now this is a thunk to make `uncons` lazier, but could force it or produce a Char# rather than a Char.
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

@\ cs -> null cs   ||   length (tail cs)  ==  length cs - 1@
-}
unsafeTail (UnsafeFromByteString bs) = UnsafeFromByteString bs'
  where
    bs' = BS.unsafeDrop (utf8LengthByLeader (BS.unsafeHead bs)) bs
{-# INLINE unsafeTail #-}

-- Honestly I'm not sure why measureOff is part of the primary Text API or even what it's supposed to do. This is my best guess from the Data.Text documentation.
measureOff :: Int -> Text -> Int
{-^ @measureOff n cs@
* if @cs@ is 'empty', is zero;
* otherwise if @cs@ contains at least @n@ 'Char's, is the number of bytes used to encode the first @n@ 'Char's in @cs@;
* otherwise it is @'length' cs@, negated.

@\ n cs -> (null cs &&  m == 0) ||  (measureOff n cs == lengthWord8 (take n cs)  &&  n <= length cs) ||  (measureOff n cs == negate (length cs)  &&  n > length cs)@
-}
measureOff n (UnsafeFromByteString bs) = measureOff_loop 0 0
  where
    measureOff_loop !charCount !byteCount
        | charCount >= n  -- n Chars have successfully been counted.
        = byteCount
        | byteCount >= BS.length bs  -- The end of the Text was reached without counting enough Chars.
        = negate charCount
        | otherwise
        = measureOff_loop
            (charCount + 1)
            (byteCount + utf8LengthByLeader (BS.unsafeIndex bs byteCount))
-- TODO: Check if it's more efficient to loop over every byte and count only leaders.

------ Operations on the underlying 'ByteString': ------

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

-- Can't name this 'fromString', so we'll keep ByteString's name until we export it in a Builder module.
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
decodeUtf8 :: BS.ByteString -> Text
decodeUtf8 bs
        | isValidUtf8 bs = UnsafeFromByteString bs
        | otherwise = error "decodeUtf8: ByteString is not UTF-8"
    -- TODO: Use a proper unicodeError.

isValidUtf8 :: BS.ByteString -> Bool
#if MIN_VERSION_bytestring(0,11,5)
-- The earlier version was buggy.
isValidUtf8 = BS.isValidUtf8
#else
-- isValidUtf8 bs = I# ( countUtf8Bytes# bs) == BS.length bs
isValidUtf8 bs = countUtf8BytesSlow bs == BS.length bs
#endif

{-
spanUtf8 :: BS.ByteString -> (Text, BS.ByteString)
{-^ O(n)
@spanUtf8 bs@ splits @bs@ into a contiguous (possibly empty) prefix of UTF-8 text and the (possibly empty) non-UTF-8 remainder of @bs@.
-}
spanUtf8 bs = (txt, bs')
  where
    i = I# ( countUtf8Bytes# bs )
    !txt = UnsafeFromByteString (BS.unsafeTake i bs)
    !bs' = BS.unsafeDrop i bs
{-# INLINE spanUtf8 #-}
-}

{- Note: What to consider an invalid code point or byte sequence
A leader without enough followers: The leader and its followers are one invalid byte sequence. (This can be up to 3 bytes.)
A follower without a leader: The lone follower is one invalid byte sequence (exactly 1 byte).
-}
{-
countUtf8Bytes# :: BS.ByteString -> Int#
{-^ O(n)
@countUtf8Bytes# bs# is the number of contiguous UTF-8 encoding bytes from the start of the 'ByteString' (or, equivalently, the index of the first non-UTF-8-encoding byte).
-}
countUtf8Bytes# bs = checkUtf8 0# 0#
  where
    !( I# length_bs ) = BS.length bs
    checkUtf8 :: Int# -> Int# -> Int#
    checkUtf8 !s !i
        | isTrue# (i <# length_bs)
        = let
            !( I# l) =
                -- safe because we just checked the length:
                utf8LengthByLeader (BS.unsafeIndex bs ( I# i )) - 1
          in case s of
            0#
                -- Use unsigned underflow to only test once:
                | isTrue# ( int2Word# l `leWord#` 3## ) ->
                    checkUtf8 l ( i +# 1# )
                | otherwise ->
                    i
            _
                | otherwise ->
                    case l of
                      -1# -> checkUtf8 ( s -# 1# ) ( i +# 1# )
                      _   -> i
        | otherwise
        = case s of
            0# -> i
        -- There's an incomplete code point at the end of the ByteString....
            _ -> backtrack ( i -# 1# )

    backtrack :: Int# -> Int#
    backtrack j
        -- safe because if it wasn't there we'd have failed going forward:
        | isFollower (BS.unsafeIndex bs ( I# j )) = backtrack ( j -# 1# )
        | otherwise                               = j
        -- could instead use state information from the previous loop as a hint for how far to backtrack, but this is simpler.

    isFollower w = w .&. 0xC0 == 0x80
-}

countUtf8BytesSlow :: BS.ByteString -> Int
countUtf8BytesSlow bs = loop 0
  where
    loop :: Int -> Int
    loop !i
        | i >= BS.length bs -- done; success
        = i

        | w <= 0x7F -- ASCII
        = loop (i + 1)

        | leads1 w
        , (i + 1) < BS.length bs  -- enough space for followers
        , isFollower w1
        , c <- char2 w w1
        , '\x80' <= c
        = loop (i + 2)

        | leads2 w
        , (i + 2) < BS.length bs  -- enough space for followers
        , isFollower w1 && isFollower w2
        , c <- char3 w w1 w2
        , '\x800' <= c
        = loop (i + 3)

        | leads3 w
        , (i + 3) < BS.length bs  -- Enough space for followers.
        , isFollower w1 && isFollower w2 && isFollower w3
        , c <- char4 w w1 w2 w3
        , '\x10000' <= c  &&  c <= '\x10FFFF' -- Max code point is artificially limited to max UTF-16.
        = loop (i + 4)

        | otherwise  -- Hit an invalid byte or didn't have enough room for followers.
        = i

      where
        w = BS.index bs i
        w1 = BS.index bs (i + 1)
        w2 = BS.index bs (i + 2)
        w3 = BS.index bs (i + 3)

        isFollower w = 0xC0 .&. w  ==  0x80
        leads1 w = 0xE0 .&. w  ==  0xC0
        leads2 w = 0xF0 .&. w  ==  0xE0
        leads3 w = 0xF8 .&. w  ==  0xF0


utf8LengthByLeader :: GHC.Word8 -> Int
{-^ @utf8LengthByLeader w@ is
* 0 if w is a follower
* 1 if w is ASCII
* 2 if w is a leader of 1
* 3 if w is a leader of 2
* 4 if w is a leader of 3
* 5 to 8 for invalid bytes
-}
utf8LengthByLeader w8 = GHC.I# (n `GHC.xorI#` (n <=# 0#))
  where
    !(GHC.I# n) = countLeadingZeros (complement w8)
{-# INLINE utf8LengthByLeader #-}

{-
utf8Length :: Char -> Int
utf8Length ( GHC.C# c# ) = GHC.I# ( utf8Length# c# )
-}

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
    4# -> case unChar4# c of (# w0, w1, w2, w3 #) -> CharBytes4 w0 w1 w2 w3
    _ -> errorWithoutStackTrace "charBytes: bad Char"
{-# INLINABLE charBytes #-}

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

char1 :: Word8 -> Char
char1 w0 = chr (fromIntegral w0)
{-# INLINE char1 #-}

char2 :: Word8 -> Word8 -> Char
char2 w0 w1 = chr $ w0' .|. w1'
  where
    !w0' = unsafeShiftL (fromIntegral (0x3F .&. w0)) 6
    !w1' = fromIntegral (unmarkTail w1)

char3 :: Word8 -> Word8 -> Word8 -> Char
char3 w0 w1 w2 = chr $ w0' .|. w1' .|. w2'
  where
    !w0' = unsafeShiftL (fromIntegral (0xF .&. w0)) 12
    !w1' = unsafeShiftL (fromIntegral (unmarkTail w1)) 6
    !w2' = fromIntegral (unmarkTail w2)

char4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
char4 w0 w1 w2 w3 =
    chr $ (w0' .|. w1') .|. (w2' .|. w3')
  where
    !w0' = unsafeShiftL (fromIntegral (0xF .&. w0)) 18
    !w1' = unsafeShiftL (fromIntegral (unmarkTail w1)) 12
    !w2' = unsafeShiftL (fromIntegral (unmarkTail w2)) 6
    !w3' = fromIntegral (unmarkTail w3)


{-
intToWord :: Int -> GHC.Word
intToWord = fromIntegral
{-# INLINE intToWord #-}

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

-- toForeignPtr0 :: BS.ByteString -> (ForeignPtr, Int)
-- toForeignPtr0 bs =
--     case IBS.toForeignPtr bs of
--         (fp, off, len) -> let !fp' = plusForeignPtr fp off in (fp', len)


{- Note: Security Model
https://unicode.org/reports/tr36/
"The byte sequence E3 80 22 is malformed because 0x22 is not a valid second trailing byte following the leading byte 0xE3. Some conversion code may report the three-byte sequence E3 80 22 as one illegal sequence and continue converting the rest, while other conversion code may report only the two-byte sequence E3 80 as an illegal sequence and continue converting with the 0x22 byte which is a syntax character in HTML and XML (U+0022 double quote). Implementations that report the 0x22 byte as part of the illegal sequence can be exploited for cross-site-scripting (XSS) attacks.
Therefore, an illegal byte sequence must not include bytes that encode valid characters or are leading bytes for valid characters.

In a reported illegal byte sequence, do not include any non-initial byte that encodes a valid character or is a leading byte for a valid sequence."

In other words, when parsing, read until the next leader regardless of how soon it appears.
-}
