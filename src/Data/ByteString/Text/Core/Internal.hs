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

{- Note: Much of the Data.Text API uses Char where it should use Text.
-}

module Data.ByteString.Text.Core.Internal (
Text (..),
-- * Construct:
pack, singleton,
append, concat, empty,
decodeUtf8, decodeUtf8Lenient,
-- * Consume:
unpack,
foldr,
uncons, unsnoc,
-- * Summarize:
null,
-- measureOff,
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
defaultChunkSize, smallChunkSize,
-- * Helpers:
replacementCharacter,
isValidUtf8,
CharBytes(..), charBytes,
foldrCharBytes,
) where

import Data.ByteString.Text.Core.Internal.Prelude
import Data.ByteString.Text.Core.Internal.Utf8

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as IBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder

import Control.DeepSeq (NFData)

import qualified GHC.Exts as GHC
import GHC.Exts
    ( IsString (..)
    , IsList (..)
    )
-- import qualified GHC.Word as GHC

import Data.Bits
    ( (.&.), (.|.), complement, countLeadingZeros
    , unsafeShiftL, unsafeShiftR,
    )
import Data.Coerce (coerce)
import Foreign.Storable (poke, pokeByteOff, sizeOf)
import qualified Foreign.ForeignPtr as FP
import qualified GHC.ForeignPtr as GHC.FP
import System.IO.Unsafe (unsafeDupablePerformIO)

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
    !n' = min defaultChunkSize (n * 4)
    -- Maximum number of bytes in a packed string of n Char is n * 4. Should this instead optimistically try n * 3?
{-# INLINE packN #-}

singleton :: Char -> Text
singleton c
    | isUtf16Surrogate c
    = replacementCharacter
    | len == 1
    = UnsafeFromByteString (BS.singleton (unChar1 c))
    | len == 2
    = case unChar2# c of
        (# w0, w1 #) ->
            unsafeDupablePerformIO $ do
                fp <- FP.mallocForeignPtrBytes len
                FP.withForeignPtr fp $ \ ptr -> do
                    poke ptr w0
                    pokeByteOff ptr 1 w1
                pure $! unsafeUnpackForeignPtrLen len fp
    | len == 3
    = case unChar3# c of
        (# w0, w1, w2 #) ->
            unsafeDupablePerformIO $ do
                fp <- FP.mallocForeignPtrBytes len
                FP.withForeignPtr fp $ \ ptr -> do
                    poke ptr w0
                    pokeByteOff ptr 1 w1
                    pokeByteOff ptr 2 w2
                pure $! unsafeUnpackForeignPtrLen len fp
    | len == 4
    = case unChar4# c of
        (# w0, w1, w2, w3 #) ->
            unsafeDupablePerformIO $ do
                fp <- FP.mallocForeignPtrBytes len
                FP.withForeignPtr fp $ \ ptr -> do
                    poke ptr w0
                    pokeByteOff ptr 1 w1
                    pokeByteOff ptr 2 w2
                    pokeByteOff ptr 3 w3
                pure $! unsafeUnpackForeignPtrLen len fp
    | otherwise
    = errorWithoutStackTrace $
          "Data.ByteString.Text.singleton: Could not process '\\'"
       <> show c

  where
    !len = utf8Length c

replacementCharacter :: Text
{-^ the Unicode replacement character @\'\\xFFFD\'@, rendered \xFFFD
-}
replacementCharacter = unsafeUnpackAddrLen# 3# "\xEF\xBF\xBD"#

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

uncons :: Text -> Maybe (Char, Text)
{-^ O(1) If the 'Text' is null, 'Nothing'; otherwise 'Just' the first 'Char' and the rest of the 'Text'.

@\ cs -> uncons cs == Just (unsafeHead cs, unsafeTail cs) ||  uncons cs == Nothing@

@uncons (fromString "儵魚出遊從容") = Just ('儵', "魚出遊從容")@
-}
uncons cs
    | null cs = Nothing
    | otherwise = Just (case uncons# cs of (# c, cs' #) -> (c, cs'))
{-# INLINE uncons #-}

unsnoc :: Text -> Maybe (Text, Char)
unsnoc cs
    | null cs   = Nothing
    | otherwise = Just (case unsnoc# cs of (# cs', c #) -> (cs', c))
{-# INLINE unsnoc #-}

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
          let !cs' = dropWord8 (GHC.I# l) cs
          in (# c, cs' #)
{-# INLINE uncons# #-}

unsnoc# :: Text -> (# Text, Char #)
{-^ O(1) The last 'Char' and the rest of the 'Text'. See also: 'unsnoc'.

WARNING: The behavior of @unsnoc# txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)
-}
unsnoc# (UnsafeFromByteString bs) =
    (# bs', chr' c #)
  where
    !bs' = UnsafeFromByteString (BS.unsafeTake length' bs)  -- forced even though it may not be used.
    !length' = end - diff
    !(diff, c)
        | w0 <= 0x7F = (0, char1' w0)
        | w1 >= 0xC0 = (1, char2' w1 w0)
        | w2 >= 0xC0 = (2, char3' w2 w1 w0)
        | otherwise  = (3, char4' w3 w2 w1 w0)
    !end = BS.length bs - 1  -- index of the last byte of the ByteString
    !w0 = BS.unsafeIndex bs end -- BS.last bs
    w1 = BS.unsafeIndex bs (end - 1)
    w2 = BS.unsafeIndex bs (end - 2)
    w3 = BS.unsafeIndex bs (end - 3)
{-# INLINABLE unsnoc# #-}

unsafeHeadLen# :: Text -> (# Char, GHC.Int# #)
{-^ O(1)
The first 'Char' of the 'Text' and the number of bytes needed to encode that 'Char' in UTF-8.

WARNING: @unsafeHeadLen# txt@ is unspecified if @null txt@ is 'True'.
-}
unsafeHeadLen# (UnsafeFromByteString bs) = (# chr' c, l #)
  where
    -- For now this is a thunk to make `uncons` lazier, but could force it or produce a Char# rather than a Char.
    c = case l of
        1# -> char1' b0
        2# -> char2' b0 b1
        3# -> char3' b0 b1 b2
        _ -> char4' b0 b1 b2 b3
    !(GHC.I# l) = utf8LengthByLeader b0
    !b0 = BS.unsafeHead bs
    b1 = BS.unsafeIndex bs 1
    b2 = BS.unsafeIndex bs 2
    b3 = BS.unsafeIndex bs 3
{-# INLINE unsafeHeadLen# #-}


unsafeTail :: Text -> Text
{-^ O(1) Drop the first 'Char' from the 'Text'. See also: 'tail', 'uncons'.

WARNING: The behavior of @unsafeTail txt@ is unspecified if @txt@ is 'null'. (Assume it will silently corrupt all your data and launch the missiles or crash the program, whichever is worse.)

@\ cs -> null cs   ||   length (tail cs)  ==  length cs - 1@
-}
unsafeTail (UnsafeFromByteString bs) = UnsafeFromByteString bs'
  where
    bs' = BS.unsafeDrop (utf8LengthByLeader (BS.unsafeHead bs)) bs
{-# INLINE unsafeTail #-}

{-
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
-}

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
charUtf8 = coerce Builder.charUtf8 . replaceBadUtf16
{-# INLINE charUtf8 #-}

fromText :: Text -> Builder
fromText = coerce Builder.byteString
{-# INLINE fromText #-}

-- Can't name this 'fromString', so we'll keep ByteString's name until we export it in a Builder module.
stringUtf8 :: [Char] -> Builder
stringUtf8 str =
    UnsafeFromBuilder (Builder.stringUtf8 (fmap replaceBadUtf16 str))
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

decodeUtf8Lenient :: BS.ByteString -> Text
{-^ Decode a 'ByteString' as UTF-8, replacing any invalid byte sequences with the Unicode replacement character @\'\\xFFFD\'@.

This follows https://unicode.org/reports/tr36/, and therefore does not exactly match the behavior of 'Data.Text.Encoding.decodeUtf8Lenient'. (At least it should match tr36 replacement option 2. If it does not, please report it as a bug.)
-}
decodeUtf8Lenient = decodeUtf8With' replaceOnError

isValidUtf8 :: BS.ByteString -> Bool
#if MIN_VERSION_bytestring(0,11,5)
-- The earlier version was buggy.
isValidUtf8 = BS.isValidUtf8
#else
-- isValidUtf8 bs = I# ( countUtf8Bytes# bs) == BS.length bs
isValidUtf8 bs = countUtf8BytesSlow bs == BS.length bs
#endif

decodeUtf8With' :: (BS.ByteString -> Builder) -> BS.ByteString -> Text
decodeUtf8With' onError bs
    | n == BS.length bs
    = UnsafeFromByteString bs
    | otherwise
    = toTextWith (BS.length bs + 2) $
           fromText (UnsafeFromByteString (BS.unsafeTake n bs))
        <> onError (BS.unsafeDrop n bs)
  where
    n = countUtf8BytesSlow bs

replaceOnError :: BS.ByteString -> Builder
{-^ Replace all maximal non-UTF-8 subsequences of the argument with @\'\\xFFFD\'@.
-}
replaceOnError bs =
    fromText replacementCharacter <> replaceOnError_loop (BS.tail bs)
  where
    replaceOnError_loop bs' =
        case spanUtf8 bs' of
            (txt, bs'')
                | BS.null bs''
                -> fromText txt
                | null txt
                -> replaceOnError_loop (BS.tail bs'') -- Do not produce another FFFD.
                | otherwise
                -> fromText txt <> replaceOnError bs'' -- Do produce another FFFD.
-- replaceOnError bs = charUtf8 '\xFFFD' <> loop 1 -- Start bad byte, which was just replaced.
--   where
--     loop !i
--         | i >= BS.length bs
--         = mempty
--         --  | isAscii' w || leads1 w || leads2 w || leads3 w
--         | utf8LengthByLeader w > 0 -- is leader, probably; spanUtf8 will do the real work of checking.
--         , (txt, bs') <- spanUtf8 (BS.drop i bs)
--         = if null txt
--             then loop (i + 1) -- Don't produce 2 consecutive FFFDs.
--             else if BS.null bs'
--                     then fromText txt
--                     else fromText txt <> replaceOnError bs'
--         | otherwise
--         = loop (i + 1)

--       where
--         w = BS.index bs i
--         isAscii' w = w <= 0x7F
--         leads1 w = 0xE0 .&. w  ==  0xC0
--         leads2 w = 0xF0 .&. w  ==  0xE0
--         leads3 w = 0xF8 .&. w  ==  0xF0



spanUtf8 :: BS.ByteString -> (Text, BS.ByteString)
{-^ O(n)
@spanUtf8 bs@ splits @bs@ into a contiguous (possibly empty) prefix of UTF-8 text and the (possibly empty) non-UTF-8 remainder of @bs@.
-}
spanUtf8 bs = (txt, bs')
  where
    i = countUtf8BytesSlow bs
    !txt = UnsafeFromByteString (BS.unsafeTake i bs)
    !bs' = BS.unsafeDrop i bs
    -- i = I# ( countUtf8Bytes# bs )
    -- !txt = UnsafeFromByteString (BS.unsafeTake i bs)
    -- !bs' = BS.unsafeDrop i bs
{-# INLINE spanUtf8 #-}

countUtf8BytesSlow :: BS.ByteString -> Int
countUtf8BytesSlow = countUtf8BytesSlowFrom 0

countUtf8BytesSlowFrom :: Int -> BS.ByteString -> Int
countUtf8BytesSlowFrom i bs = loop i
  where
    loop :: Int -> Int
    loop !i
        | i >= BS.length bs -- done; success
        = i

        | w <= 0x7F -- ASCII
        = loop (i + 1)

        | leads1 w
        , (i + 1) < BS.length bs  -- room for followers
        , isFollower w1
        , 0x80 <= char2' w w1  -- no 'overlong' encodings
        = loop (i + 2)

        | leads2 w
        , (i + 2) < BS.length bs  -- room for followers
        , isFollower w1 && isFollower w2
        , 0x800 <= char3' w w1 w2  -- no 'overlong' encodings
        = loop (i + 3)

        -- Here live 4-bit UTF-8 encoded UTF-16 surrogates (MUTF-8, UCS-2), among other things.

        | leads3 w
        , (i + 3) < BS.length bs  -- room for followers.
        , isFollower w1 && isFollower w2 && isFollower w3
        , c <- char4' w w1 w2 w3
        , 0x10000 <= c  &&  c <= 0x10FFFF -- Below 0x1000 are overlong encodings and UTF-16 surrogates; max code point is artificially limited to max UTF-16.
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


-- wordToChar :: GHC.Word -> Char
-- wordToChar x = chr (fromIntegral x)
-- {-# INLINE wordToChar #-}



{-
intToWord :: Int -> GHC.Word
intToWord = fromIntegral
{-# INLINE intToWord #-}

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

asForeignPtrLen :: Text -> (Int -> FP.ForeignPtr Word8 -> r) -> r
#if MIN_VERSION_bytestring(0,11,0)
asForeignPtrLen (UnsafeFromByteString (IBS.BS fp len)) f =
    f len fp
#else
asForeignPtrLen (UnsafeFromByteString (IBS.PS fp off len)) f =
    f len (FP.plusForeignPtr fp off)
#endif

unsafeUnpackForeignPtrLen :: Int -> FP.ForeignPtr Word8 -> Text
#if MIN_VERSION_bytestring(0,11,0)
unsafeUnpackForeignPtrLen len fp = UnsafeFromByteString (IBS.BS fp len)
#else
unsafeUnpackForeignPtrLen len fp = UnsafeFromByteString (IBS.PS fp 0 len)
#endif


-- toForeignPtr0 :: BS.ByteString -> (ForeignPtr, Int)
-- toForeignPtr0 bs =
--     case IBS.toForeignPtr bs of
--         (fp, off, len) -> let !fp' = plusForeignPtr fp off in (fp', len)

unsafeUnpackAddrLen# :: Int# -> GHC.Addr# -> Text
unsafeUnpackAddrLen# len# addr# =
#if MIN_VERSION_base(4,15,0)
    unsafeFromForeignPtrLen
        ( GHC.I# len# )
        ( GHC.FP.ForeignPtr addr# GHC.FP.FinalPtr )
#else
   UnsafeFromByteString
       (unsafeDupablePerformIO
           ( BS.unsafePackAddressLen ( GHC.I# len# ) addr# ))
#endif
{-# INLINE unsafeUnpackAddrLen# #-}

{- Note: Security Model
https://unicode.org/reports/tr36/
"The byte sequence E3 80 22 is malformed because 0x22 is not a valid second trailing byte following the leading byte 0xE3. Some conversion code may report the three-byte sequence E3 80 22 as one illegal sequence and continue converting the rest, while other conversion code may report only the two-byte sequence E3 80 as an illegal sequence and continue converting with the 0x22 byte which is a syntax character in HTML and XML (U+0022 double quote). Implementations that report the 0x22 byte as part of the illegal sequence can be exploited for cross-site-scripting (XSS) attacks.
Therefore, an illegal byte sequence must not include bytes that encode valid characters or are leading bytes for valid characters.

In a reported illegal byte sequence, do not include any non-initial byte that encodes a valid character or is a leading byte for a valid sequence."

In other words, when recovering from an error, read until the next leader regardless of how soon or late it appears.

5.22 Best Practice for U+FFFD Substitution
"When converting text from one character encoding to another, a conversion algorithm may encounter unconvertible code units. . . .
When a conversion algorithm encounters such unconvertible data, the usual practice is either to throw an exception or to use a defined substitution character to represent the unconvertible data. In the case of conversion to one of the encoding forms of the Unicode Standard, the substitution character is defined as U+FFFD replacement character.
However, there are different possible ways to use U+FFFD. This section describes the best practice.
For conversion between different encoding forms of the Unicode Standard, Section 3.9, Unicode Encoding Forms defines best practice for the use of U+FFFD. The basic formulation is as follows:

Whenever an unconvertible offset is reached during conversion of a code
unit sequence:

1. The maximal subpart at that offset should be replaced by a single
U+FFFD.

2. The conversion should proceed at the offset immediately after the maximal subpart.

In that formulation, the term “maximal subpart” refers to a maximal subpart of an illformed subsequence, which is precisely defined in Section 3.9, Unicode Encoding Forms for Unicode encoding forms. Essentially, a conversion algorithm gathers up the longest sequence of code units that could be the start of a valid, convertible sequence, but which is not actually convertible. For example, consider the first three bytes of a four-byte UTF-8
sequence, followed by a byte which cannot be a valid continuation byte: <F4 80 80 41>. In that case <F4 80 80> would be the maximal subpart that would be replaced by a single U+FFFD. If there is not any start of a valid, convertible sequence in the unconvertible data at a particular offset, then the maximal subpart would consist of a single code unit."
-}
