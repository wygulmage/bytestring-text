{-# LANGUAGE NoImplicitPrelude
           , CPP
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , MagicHash
           , BangPatterns
           , UnboxedTuples
  #-}

{-# OPTIONS_HADDOCK not-home
  #-}

module Data.ByteString.Text.Short.Internal (
ShortText (..),
empty, append, concat,
pack,
unpack, foldr,
replicate,
lengthWord8,
dropWord8, takeWord8,
) where

import qualified Data.ByteString.Text.Builder.Internal as Builder
import qualified Data.ByteString.Text.Builder.Internal.Utf8 as Utf8
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as ILBS
import qualified Data.ByteString.Short.Internal as IBS

import GHC.Base hiding (empty, foldr)
import GHC.Num

import qualified GHC.Exts as GHC

import Control.Monad.ST
import Foreign.ForeignPtr
import Text.Read
import Text.Show


newtype ShortText = SBS BS.ShortByteString
  deriving newtype
    ( Monoid, Semigroup
    , Ord, Eq
    )

instance Read ShortText where
    readPrec = fmap pack readPrec
    readList = readListDefault
    readListPrec = readListPrecDefault

instance Show ShortText where
    showsPrec _ = showList . unpack

empty :: ShortText
empty = mempty
{-# INLINE empty #-}

append :: ShortText -> ShortText -> ShortText
append = (<>)
{-# INLINE append #-}

concat :: [ShortText] -> ShortText
concat = mconcat
{-# INLINE concat #-}

unpack :: ShortText -> [Char]
unpack txt = build (\ cons nil -> foldr cons nil txt)
{-# INLINE [~0] unpack #-}

lengthWord8 :: ShortText -> Int
lengthWord8 = coerce BS.length
{-# INLINE lengthWord8 #-}

takeWord8 :: Int -> ShortText -> ShortText
#if MIN_VERSION_bytestring(0,11,3)
takeWord8 = coerce SBS.take
#else
takeWord8 n txt
    | n <= 0    = empty
    | n >= len  = txt
    | otherwise = sliceWord8 0 n txt
  where
    !len = lengthWord8 txt
#endif

dropWord8 :: Int -> ShortText -> ShortText
#if MIN_VERSION_bytestring(0,11,3)
dropWord8 = coerce SBS.drop
#else
dropWord8 n txt
    | n >= len  = empty
    | n == 0    = txt
    | otherwise = sliceWord8 n (len - n) txt
  where
    !len = lengthWord8 txt
#endif

sliceWord8 :: Int -> Int -> ShortText -> ShortText
{-^ @sliceWord8 offset size txt@ creates a new @ShortText@ that consists of @size@ bytes of @txt@, starting at byte offset @offset@.

WARNING: This can create invalid (non-UTF-8) 'ShortText' values, and the result is unspecified if @'lengthWord8' txt@ < @offset + size@, or if @offset@ or @size@ is negative. Use with extreme caution.
-}
sliceWord8 ( I# off# ) ( I# n# ) txt@(SBS ba@( IBS.SBS ba# ))
    = runRW# $ \ s0 ->
        case newByteArray# n# s0 of
            (# s1, mba# #) ->
                case copyByteArray# ba# off# mba# 0# n# s1 of
                    s2 ->
                        case unsafeFreezeByteArray# mba# s2 of
                            (# _, ba'# #) ->
                                SBS ( IBS.SBS ba'# )
{- Could throw in some sanity checks for sliceWord8:
sliceWord8 _   n _   | n <= 0                        = empty
sliceWord8 off _ txt | off >= length txt             = empty
sliceWord8 off n txt | off <= 0  &&  n >= length txt = txt

Although these sanity checks match sliceWord8 off n = take n . drop off, they may provide counter-intuitive results when offset or n is out-of-bounds.
-}

replicate :: Int -> ShortText -> ShortText
{-^ @replicate n txt@ O(n * length txt)
prop> \ n txt -> replicate n txt == concat (List.repeat n txt)

If @n@ = 1, @replicate n txt@ is @txt@; if @n@ <= 0, it is 'empty'. Otherwise, it produces a completely new 'ShortText'.
-}
#if MIN_VERSION_bytestring(0,11,3)
replicate = coerce SBS.replicate
#else
replicate n@( I# n# ) txt@(SBS ( IBS.SBS ba# ))
    | n <= 0  -- Match Data.List's behavior for counts less than 0 rather than throwing an error. (This is questionable.)
    = empty
    | n == 1
    = txt
    | otherwise
    = runRW# $ \ s0 ->
        case newByteArray# len'# s0 of
            (# s1, mba# #) ->
              case
                unsafeFreezeByteArray#
                    mba#
                    ( replicate_loop mba# s1 0# )
              of
                  (# _, ba'# #) -> SBS ( IBS.SBS ba'# )
  where
    !len# = sizeofByteArray# ba#
    !len'# = n# *# len#
    replicate_loop mba# s i#
        | isTrue# ( i# <# len'# )
        = case copyByteArray# ba# 0# mba# i# len# s of
              s' -> replicate_loop mba# s' ( i# +# len# )
        | otherwise
        = s
#endif


instance GHC.IsString ShortText where
    fromString = pack
    {-# INLINE fromString #-}

pack :: [Char] -> ShortText
pack str =
    case Builder.fromString str of
      Builder.BSB bsb ->
          unsafeFromLazyByteString (BSB.toLazyByteString bsb)
{-# INLINE [~0] pack #-}

unsafeFromLazyByteString :: LBS.ByteString -> ShortText
unsafeFromLazyByteString lbs = SBS (BS.toShort (LBS.toStrict lbs))

unsafeIndexWord8# :: ShortText -> Int -> (# Char, Int #)
unsafeIndexWord8# (SBS sbs) = Utf8.unsafeIndexLenVia (BS.index sbs)
{-# INLINE unsafeIndexWord8# #-}

foldr :: (Char -> b -> b) -> b -> ShortText -> b
foldr f z = \ txt ->
  let
    !len = lengthWord8 txt
    foldr_go !i
        | i < len
        = case unsafeIndexWord8# txt i of
            (# c, off #) ->
                c `f` foldr_go (i + off)
        | otherwise
        = z
  in foldr_go 0

{-
unsafeFromLazyByteString :: LBS.ByteString -> ShortText
{-^ Convert a 'LBS.ByteString' to a 'ShortText'. If the lazy byte string is not UTF-8 encoded, terrible things will happen.
-}
-- definition adapted from Data.ByteString.Lazy. Once there's a 'toShort' function there, we can just use that.
unsafeFromLazyByteString = \ lbs -> goLen0 lbs lbs
  where
    goLen0 _ ILBS.Empty = empty
    goLen0 lbs0 (ILBS.Chunk bs lbs') = goLen1 lbs0 bs lbs'

    goLen1 _ bs Empty = BS.toShort bs
    goLen1 lbs0 (IBS.PS _ _ len) (ILBS.Chunk (IBS.PS _ _ len') lbs') =
        goLen
            lbs0
            (addLen len len')
            lbs'

    goLen lbs0 !total (ILBS.Chunk (S.PS _ _ len) lbs') =
        goLen lbs0 (addLen total len) lbs'
    golen lbs0 total ILBS.Empty =
        SBS.create total $ \ mba -> goCopy lbs0 mba 0

    goCopy ILBS.Empty !_ !_ = pure ()
    goCopy (ILBS.Chunk (IBS.PS fp off' len) lbs') mba off =
      let !off'' = off + off'
      in do
          unsafeWithForeignPtrST $ \ ftr ->
              SBS.copyAddrToByteArray ptr mba (off'') len
          goCopy lbs' mba off''

    addLen = IBS.checkedAdd "unsafeLazyFromByteString"


unsafeWithForeignPtrST :: ForeignPtr a -> (Ptr a -> ST b) -> ST b
unsafeWithForeignPtrST fp f =
    unsafeIOToST (unsafeWithForeignPtr fp (unsafeSTToIO . f))
-}
