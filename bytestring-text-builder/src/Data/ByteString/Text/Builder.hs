
{-| Basic 'Builder' definition. For more useful functions, see 'Data.ByteString.Text' or 'Data.ByteString.Text.Short'
-}
module Data.ByteString.Text.Builder (
Builder, toByteStringBuilder,
fromString,
singleton,
defaultChunkSize, smallChunkSize,
) where

import Data.ByteString.Text.Builder.Internal
