
{-| This module exports functions that have unspecified behavior for certain arguments.
-}

module Data.ByteString.Text.Core.Unsafe (
unsafeHead, unsafeTail,
lengthWord8,
takeWord8,
dropWord8,
) where

import Data.ByteString.Text.Core.Internal
