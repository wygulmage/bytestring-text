{-# LANGUAGE NoImplicitPrelude
  #-}

{-| This module exists to avoid name clashes created by importing 'Prelude' unqualified.
-}
module Data.ByteString.Text.Core.Internal.Prelude (
HasCallStack, assert,
module GHC.Enum,
module GHC.Err,
module GHC.Num,
module GHC.Real,
module Data.Bool,
module Data.Char,
module Data.Eq,
module Data.Function,
module Data.Functor,
module Data.Int,
module Data.Ord,
module Data.List.NonEmpty,
module Data.Maybe,
module Data.Monoid,
module Data.Semigroup,
module Data.Tuple,
module Data.Word,
module Text.Read,
module Text.Show,
) where

import GHC.Enum
import GHC.Err (error, errorWithoutStackTrace, undefined)
import GHC.Num hiding (quotRemInteger)
import GHC.Real (fromIntegral)
import GHC.Stack (HasCallStack)

import Control.Exception (assert)

import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.Ord
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Tuple
import Data.Word

import Text.Read
    ( Read (readPrec, readList, readListPrec)
    , readListDefault, readListPrecDefault
    )
import Text.Show
