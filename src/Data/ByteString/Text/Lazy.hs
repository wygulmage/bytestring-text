{-# LANGUAGE NoImplicitPrelude
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , TypeFamilies
  #-}

{-| This module is very much a work-in-progress and is not ready for export.
At the most basic level, I'm not sure whether a (lazy) Text should be considered a (lazy) ByteString that encodes UTF-8 text, or a (lazy) ByteString where each chunk represents a strict Text. The difference is significant: All of ByteString is set up for the first interpretation, but Data.Text.Lazy is represented the second way. The first makes production and conversion to strict Text simpler, while the second makes use as lazy Text simpler.
-}

module Data.ByteString.Text.Lazy (
Text,
-- * Creation:
pack,
singleton,
-- empty,
append,
concat,
-- * Consumption:
unpack,
-- uncons,
-- unsnoc,
-- head,
-- last,
-- tail,
-- init,
-- * Conversion:
-- toStrict,
-- fromStrict,
-- * Summary:
-- null,
-- compareLength,
-- length,
foldr,
foldrChunks,
) where

import qualified Data.ByteString.Text as Strict
import qualified Data.ByteString.Text.Core.Internal as Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString(..))
import qualified Data.ByteString.Builder as Builder
import Control.DeepSeq (NFData)
import qualified GHC.Exts as GHC
import GHC.Exts
    ( IsString (..)
    , IsList (..)
    , Int, ( +# ), ( <=# )
    )
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import qualified GHC.Word as GHC
import GHC.Word (Word8)
import Data.Functor (fmap)
import Data.Monoid (Monoid, mempty, mconcat)
import Data.Semigroup (Semigroup, (<>))
import Data.Bits
    ((.&.), (.|.), complement, countLeadingZeros, shift)
import Data.Ord (Ord (..))
import Data.Eq (Eq (..))
import Data.Bool (Bool (..), otherwise)
import Data.Char
import Data.Function
import Data.Maybe (Maybe (..))
import Data.Coerce (coerce)
import Text.Read
    ( Read (readPrec, readList, readListPrec)
    , readListDefault, readListPrecDefault
    )
import Text.Show

newtype Text = UnsafeFromByteString LBS.ByteString
  deriving newtype
    ( Ord, Eq
    , Monoid, Semigroup
    , NFData
    )

encodeUtf8 :: Text -> LBS.ByteString
encodeUtf8 (UnsafeFromByteString bs) = bs

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
    fromList = pack
    {-# INLINE fromList #-}
    toList = unpack
    {-# INLINE toList #-}

pack :: [Char] -> Text
pack =
    UnsafeFromByteString
  . Builder.toLazyByteString
  . Builder.stringUtf8
{-# INLINE [~0] pack #-}

singleton :: Char -> Text
singleton c
    -- | isAscii c
    -- = UnsafeFromByteString (LBS.singleton (fromIntegral (ord c)))
    -- | otherwise
    = UnsafeFromByteString (Builder.toLazyByteString (Builder.charUtf8 c))
{-# INLINE [~0] singleton #-}

append :: Text -> Text -> Text
append = (<>)
{-# INLINE append #-}

concat :: [Text] -> Text
concat = mconcat
{-# INLINE concat #-}

unpack :: Text -> [Char]
unpack cs = GHC.build (\ cons nil -> foldr cons nil cs)
{-# INLINE [~0] unpack #-}

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f = foldrChunks (\ cs z -> Strict.foldr f z cs)

foldrChunks :: forall a. (Strict.Text -> a -> a) -> a -> Text -> a
foldrChunks =
    coerce (LBS.foldrChunks :: (BS.ByteString -> a -> a) -> a -> LBS.ByteString -> a)
{-# INLINE foldrChunks #-}

unconsChunk :: Text -> Maybe (Strict.Text, Text)
unconsChunk (UnsafeFromByteString css) = case css of
    LBS.Chunk cs css' -> Just (Strict.UnsafeFromByteString cs, UnsafeFromByteString css')
    LBS.Empty -> Nothing
