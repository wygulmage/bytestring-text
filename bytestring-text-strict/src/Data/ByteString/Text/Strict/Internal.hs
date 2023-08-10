{-# LANGUAGE NoImplicitPrelude
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , TypeFamilies
           , CPP
  #-}

{-# OPTIONS_HADDOCK not-home
  #-}


module Data.ByteString.Text.Strict.Internal (
Text (..),
pack, fromBuilder,
unpack, uncons, foldr,
lengthWord8,
-- For testing:
indices, indicesBS, indicesBrutalBS, indicesTwoWayBS,
) where

import Data.ByteString.Text.Builder.Internal.Prelude
import qualified Data.ByteString.Text.Builder.Internal as Builder
import Data.ByteString.Text.Builder.Internal.Search
import qualified Data.ByteString.Text.Builder.Internal.Utf8 as Utf8

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BSB
    (toLazyByteStringWith, safeStrategy)

import Control.DeepSeq (NFData)

import qualified GHC.Exts as GHC (IsString (..), build)
import Data.Coerce (coerce)


newtype Text = TextBS BS.ByteString
  deriving newtype
    ( Monoid, Semigroup
    , Ord, Eq
    , NFData
    )

instance Read Text where
    readPrec = fmap pack readPrec
    readList = readListDefault
    readListPrec = readListPrecDefault

instance Show Text where
    showsPrec _ = showList . unpack

instance GHC.IsString Text where
    fromString = pack

pack :: [Char] -> Text
pack = fromBuilder . Builder.fromString
{-# INLINE [~0] pack #-}

unpack :: Text -> [Char]
unpack txt = GHC.build (\ cons nil -> foldr cons nil txt)
{-# INLINE unpack #-}

unsafeHead :: Text -> Char
unsafeHead (TextBS bs) =
    case Utf8.unsafeIndexNextVia (BS.unsafeIndex bs) 0 of
        (# c, _ #) -> c

uncons :: Text -> Maybe (Char, Text)
uncons (TextBS bs)
    | BS.null bs
    = Nothing
    | otherwise
    = case Utf8.unsafeIndexNextVia (BS.unsafeIndex bs) 0 of
        (# c, off #) -> Just (c, TextBS (BS.drop off bs))

foldr :: (Char -> b -> b) -> b -> Text -> b
foldr = Utf8.foldrIndexLen (coerce BS.unsafeIndex) lengthWord8
{-# INLINE [0] foldr #-}

fromBuilder :: Builder.Builder -> Text
fromBuilder = fromBuilderWith Builder.smallChunkSize

fromBuilderWith :: Int -> Builder.Builder -> Text
{-^ O(n)
Convert a 'Builder' to a 'Text'.
The @Int@ is the first buffer size, and should be your best estimate of the size of the result 'Text'.
-}
fromBuilderWith sizeHint bldr =
    TextBS (LBS.toStrict (BSB.toLazyByteStringWith
        (BSB.safeStrategy sizeHint Builder.defaultChunkSize)
        LBS.empty
        (Builder.toByteStringBuilder bldr)))
{-# INLINE fromBuilderWith #-}

lengthWord8 :: Text -> Int
lengthWord8 = coerce BS.length
{-# INLINE lengthWord8 #-}

indices :: Text -> Text -> [Int]
indices = coerce indicesBS
{-# INLINE indices #-}

indicesBS :: BS.ByteString -> BS.ByteString -> [Int]
indicesBS pat
    | BS.length pat == 0
    = allIndices
    | BS.length pat <= 32
    = indicesBrutalBS pat
    | otherwise
    = indicesTwoWayBS pat
  where
    allIndices txt = loop 0 (BS.length txt)
    loop !i !len
        | i < len = i : loop (i + 1) len
        | otherwise = []
{-# INLINABLE indicesBS #-}

indicesBrutalBS :: BS.ByteString -> BS.ByteString -> [Int]
{-^ Warning: This does not work for empty needles! -}
indicesBrutalBS !pat !txt =
    loop 0
  where
    !l_pat = BS.length pat
    !end = BS.length txt - l_pat
    loop !i
        | i  <=  end
        = let next = loop (i + 1)
          in if unsafeSliceBS i l_pat txt == pat
            then i : next
            else next
        | otherwise
        = []
{-# NOTINLINE indicesBrutalBS #-}

unsafeSliceBS :: Int -> Int -> BS.ByteString -> BS.ByteString
unsafeSliceBS off len bs = BS.take len (BS.drop off bs)

indicesTwoWayBS !pat =
    indicesTwoWayVia
        (\ bs1 off1 bs2 off2 len ->
            unsafeSliceBS off1 len bs1 == unsafeSliceBS off2 len bs2)
        BS.unsafeIndex
        BS.length
        pat
{-# NOTINLINE indicesTwoWayBS #-}
