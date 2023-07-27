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
isPrefixOf, isSuffixOf,
cycleN,
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
{-^ @takeWord8 n txt@ takes the first @n@ bytes from @txt@.

This is extremely unsafe! The result is unspecified if you take
 * part of a multibyte code point
 * more bytes than you have
 * a negative number of bytes
-}
#if MIN_VERSION_bytestring(0,11,3)
takeWord8 = coerce SBS.take
#else
takeWord8 n txt = sliceWord8 0 n txt
#endif

dropWord8 :: Int -> ShortText -> ShortText
{-^ @dropWord8 n txt@ drop the first @n@ bytes from @txt@.

This is extremely unsafe! The result is unspecified if you drop
 * part of a multibyte code point
 * more bytes than you have
 * a negative number of bytes
-}
#if MIN_VERSION_bytestring(0,11,3)
dropWord8 = coerce SBS.drop
#else
dropWord8 n txt = sliceWord8 n (lengthWord8 txt - n) txt
#endif

isPrefixOf :: ShortText -> ShortText -> Bool
#if MIN_VERSION_bytestring(0,11,3)
isPrefixOf = coerce BS.isPrefixOf
{-# INLINE isPrefixOf #-}
#else
isPrefixOf pre txt =
    len_pre <= lengthWord8 txt
    &&  compareWord8Slices pre 0 txt 0 len_pre == EQ
  where
    !len_pre = lengthWord8 pre
{-# NOTINLINE isPrefixOf #-}
#endif

isSuffixOf :: ShortText -> ShortText -> Bool
#if MIN_VERSION_bytestring(0,11,3)
isSuffixOf = coerce BS.isSuffixOf
{-# INLINE isSuffixOf #-}
#else
isSuffixOf suf txt =
    off_txt >= 0
    &&  compareWord8Slices suf 0 txt off_txt len_suf == EQ
    -- If null suf, off_txt is lengthWord8 txt, which is past the last item -- but n is 0, so no items should be compared anyway.
  where
    !len_suf = lengthWord8 suf
    !off_txt = lengthWord8 txt - len_suf
{-# NOTINLINE isSuffixOf #-}
#endif

compareWord8Slices ::
    ShortText -> Int -> ShortText -> Int -> Int -> Ordering
compareWord8Slices
    (SBS ( IBS.SBS x )) ( I# off_x )
    (SBS ( IBS.SBS y )) ( I# off_y )
    ( I# n )
    = case compareByteArrays# x off_x y off_y n of
        sign
            -- Matching the default 'compare' definition lets GHC make slightly better code for the *fixOf functions.
            | isTrue# ( sign ==# 0# ) -> EQ
            | isTrue# ( sign <=# 0# ) -> LT
            | otherwise               -> GT

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

cycleN :: Int -> ShortText -> ShortText
{-^ @cycleN n txt@ O(n * length txt)
prop> \ n txt -> cycleN n txt == concat (List.repeat n txt)

prop> \ n txt -> cycleN n txt == mtimesDefault n txt

If @n@ = 1, @cycleN n txt@ is @txt@; if @n@ <= 0, it is 'empty'. Otherwise, it produces a completely new 'ShortText'.
-}
#if MIN_VERSION_bytestring(0,11,3)
cycleN = coerce SBS.cycleN
#else
cycleN n@( I# n# ) txt@(SBS ( IBS.SBS ba# ))
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
                    ( cycleN_loop mba# s1 0# )
              of
                  (# _, ba'# #) -> SBS ( IBS.SBS ba'# )
  where
    !len# = sizeofByteArray# ba#
    !len'# = n# *# len#
    cycleN_loop mba# s i#
        | isTrue# ( i# <# len'# )
        = case copyByteArray# ba# 0# mba# i# len# s of
              s' -> cycleN_loop mba# s' ( i# +# len# )
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

-- unsafeIndexWord8# :: ShortText -> Int -> (# Char, Int #)
-- unsafeIndexWord8# (SBS sbs) = Utf8.unsafeIndexNextVia (BS.index sbs)
-- {-# INLINE unsafeIndexWord8# #-}

foldr :: (Char -> b -> b) -> b -> ShortText -> b
foldr = Utf8.foldrIndexLen (coerce BS.index) lengthWord8
{-# INLINE [0] foldr #-}

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
