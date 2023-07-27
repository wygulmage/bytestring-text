{-# LANGUAGE NoImplicitPrelude
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
  #-}

{-# OPTIONS_HADDOCK not-home
  #-}

module Data.ByteString.Text.Builder.Internal (
Builder (..), toByteStringBuilder,
fromString,
singleton,
replaceBadUtf16,
defaultChunkSize, smallChunkSize,
) where

import Data.ByteString.Text.Builder.Internal.Prelude

import Data.ByteString.Text.Builder.Internal.Utf8
import qualified Data.ByteString.Builder as BSB
import qualified GHC.Exts as GHC
import Foreign.Storable (sizeOf)

newtype Builder = BSB BSB.Builder
  deriving newtype
    (Monoid, Semigroup)
{-^ @Builder@ is an efficient way to incrementally build a 'Text'.
-}

toByteStringBuilder :: Builder -> BSB.Builder
{-^ O(1) Convert a 'Builder' to 'BSB.Builder' that produces UTF-8-encoded output.
-}
toByteStringBuilder (BSB bsb) = bsb

instance GHC.IsString Builder where
    fromString = fromString

instance Eq Builder where
    BSB k1 == BSB k2 =
        BSB.toLazyByteString k1 == BSB.toLazyByteString k2

instance Ord Builder where
    BSB k1 `compare` BSB k2 =
        BSB.toLazyByteString k1 `compare` BSB.toLazyByteString k2

singleton :: Char -> Builder
singleton c = BSB (BSB.charUtf8 (replaceBadUtf16 c))
{-# INLINE singleton #-}

fromString :: [Char] -> Builder
fromString str = BSB (BSB.stringUtf8 (fmap replaceBadUtf16 str))
{-# INLINE fromString #-}

defaultChunkSize :: Int
{-^ Default chunk size in bytes. Slightly less than 16 kibibites.

This is about half (at the time of writing) the default chunk size of 'Data.ByteString.Builder.Builder'.

>>> defaultChunkSize
16368
-}
defaultChunkSize = 16 * 1024  -  chunkOverhead

smallChunkSize :: Int
{-^ Small chunk size in bytes.

>>> smallChunkSize
102
-}
smallChunkSize = 128 - chunkOverhead

chunkOverhead :: Int
{-^ GHC's memory management overhead (as of writing, according to bytestring); for a 64-bit system this is 16 bytes. -}
chunkOverhead = 2 * sizeOf (undefined :: Int)
