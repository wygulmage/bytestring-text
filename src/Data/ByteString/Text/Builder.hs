{-# LANGUAGE NoImplicitPrelude
  #-}

module Data.ByteString.Text.Builder (
Builder,
toByteStringBuilder,
toText, toTextWith,
fromText,
fromString,
singleton,
) where

import Data.ByteString.Text.Core.Internal
    ( Builder
    , toText, toTextWith
    , fromText
    , fromString
    )
import qualified Data.ByteString.Text.Core.Internal as Txt
import Data.ByteString.Text.Core.Internal.Prelude

import qualified Data.ByteString.Builder as BS (Builder)

singleton :: Char -> Builder
singleton = Txt.charUtf8
{-# INLINE singleton #-}

toByteStringBuilder :: Builder -> BS.Builder
toByteStringBuilder (Txt.UnsafeFromBuilder b) = b
