{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , NoImplicitPrelude
  #-}

module Data.ByteString.Text.Encoding.Error (
OnError,
OnDecodeError,
UnicodeException (..),
lenientDecode,
strictDecode,
ignore,
replace,
) where

import Control.DeepSeq (NFData (..))
import GHC.Base
import qualified GHC.Generics as GHC
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Data.Word
import Numeric (showHex)
import Text.Show

type OnError a b = String -> Maybe a -> Maybe b
{-^ @OnError a b@ is the type of a handler for an error that includes a description and may include a cause of type @a@. The handler may provide 'Just' a replacement value of type @b@, or 'Nothing', or it may 'throw' an 'Exception'.
-}

type OnDecodeError = [Char] -> Maybe Word8 -> Maybe Char
-- ^ a handler for a decode error

data UnicodeException = DecodeError [Char] (Maybe Word8)
  deriving (GHC.Generic, Typeable, Eq, Ord)
-- ^ an exception for Unicode decode errors

instance NFData UnicodeException

instance Exception UnicodeException

instance Show UnicodeException where
    show (DecodeError msg cause) =
        "Cannot decode " <> case cause of
            Nothing ->
                "input: " <> msg
            Just byte ->
                "byte '\\x" <> showHex byte ("': " <> msg)

lenientDecode :: OnError a Char
-- ^ Replace the invalid input with @\xfffd@ (the Unicode replacement character).
lenientDecode = replace '\xfffd'

strictDecode :: OnDecodeError
-- ^ Throw a 'UnicodeException' if decoding fails.
strictDecode msg cause = throw (DecodeError msg cause)

ignore :: OnError a b
ignore _ _ = Nothing

replace :: b -> OnError a b
replace replacement _ _ = Just replacement
