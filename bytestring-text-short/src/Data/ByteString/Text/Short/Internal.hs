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
empty, append, concat, pack, fromBuilder, replicate,
unpack, foldr,
isPrefixOf, isSuffixOf, isInfixOf,
null,
lengthWord8,
dropWord8, takeWord8,
) where

import Data.ByteString.Text.Builder.Internal.Prelude
import qualified Data.ByteString.Text.Builder.Internal as Builder
import qualified Data.ByteString.Text.Builder.Internal.Utf8 as Utf8
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as ILBS
import qualified Data.ByteString.Short.Internal as IBS

import GHC.Base hiding (empty, foldr)

import qualified GHC.Exts as GHC

import Control.Monad.ST
import qualified Data.List as List
import Foreign.ForeignPtr
import Text.Read
import Text.Show


newtype ShortText = SBS BS.ShortByteString
  deriving newtype
    ( Monoid
    )


-- Local compareWord8Slices is used here for comparison functions, since we define it anyway. Should things be switched to use Short.ByteString functions instead?

instance Ord ShortText where
    compare = cmpShortText
    {-# INLINE compare #-}

cmpShortText :: ShortText -> ShortText -> Ordering
cmpShortText !txt1 !txt2 =
    compareWord8Slices txt1 0 txt2 0 (min len1 len2) <> compare len1 len2
  where
    !len1 = lengthWord8 txt1
    !len2 = lengthWord8 txt2
{-# INLINABLE cmpShortText #-}


instance Eq ShortText where
    txt1 == txt2 =
        len1 == lengthWord8 txt2
        &&  compareWord8Slices txt1 0 txt2 0 len1 == EQ
      where
        !len1 = lengthWord8 txt1
    {-# INLINABLE (==) #-}

instance Semigroup ShortText where
    (<>) = append
    {-# INLINE (<>) #-}

    sconcat (txt :| txts) = concat (txt : txts)
    {-# INLINE sconcat #-}

    stimes n txt
        | lenW8' <= max_Int
        = replicate (fromIntegral n) txt
        | otherwise = errorWithoutStackTrace "stimes: overflow"
      where
        !lenW8' = toInteger n * toInteger (lengthWord8 txt)
        !max_Int = toInteger max_Int'
          where
            max_Int' :: Int
            !max_Int' = maxBound
    {-# INLINE stimes #-}

append :: ShortText -> ShortText -> ShortText
append = mappend
{-# INLINE append #-}

instance Read ShortText where
    readPrec = fmap pack readPrec
    readList = readListDefault
    readListPrec = readListPrecDefault

instance Show ShortText where
    showsPrec _ = showList . unpack

empty :: ShortText
empty = mempty
{-# INLINE empty #-}

concat :: [ShortText] -> ShortText
concat = mconcat
{-# INLINE concat #-}

unpack :: ShortText -> [Char]
unpack txt = build (\ cons nil -> foldr cons nil txt)
{-# INLINE [~0] unpack #-}

null :: ShortText -> Bool
null = coerce BS.null
{-# INLINE null #-}

lengthWord8 :: ShortText -> Int
{-^ the number of bytes used to encode the UTF-8
-}
lengthWord8 = coerce BS.length
{-# INLINE lengthWord8 #-}

takeWord8 :: Int -> ShortText -> ShortText
{-^ @takeWord8 n txt@ is the first @n@ bytes of @txt@.

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
{-^ @dropWord8 n txt@ is @txt@ without the first @n@ bytes.

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

isInfixOf :: ShortText -> ShortText -> Bool
#if MIN_VERSION_bytestring(0,11,3)
isInfixOf = coerce BS.isInfixOf
{-# INLINE isInfixOf #-}
#else
isInfixOf = isInfixOfBrutal
#endif

isInfixOfBrutal :: ShortText -> ShortText -> Bool
{-^ O(min 1 (length needle) * length haystack)
Compare all substrings of haystack of length (length needle) to needle; if one is equal, evaluate to True; otherwise False.

isInfixOf "" "" is True because "" is a substring of "".
indices "" "" is [] because "" has no indices.
So isInfixOf needle haystack is not equivalent to not (List.null needle haystack).
-}
isInfixOfBrutal needle haystack =
    go (lengthWord8 haystack - l_needle)
    -- Starting with the rightmost, compare all substrings of haystack that are as long as needle to needle. If one matches, evaluate to True; otherwise evaluate to False.
    -- This doesn't need a null needle check. It'll memcmp nothing to nothing once and evaluate to True. Which is plenty efficient for a useless edge case.
  where
    !l_needle = lengthWord8 needle
    go !i
      | i >= 0
      = case compareWord8Slices needle 0 haystack i l_needle of
            EQ -> True
            _  -> go (i - 1)
      | otherwise
      = False
{-# INLINABLE isInfixOfBrutal #-}

-- indicesBrutal :: ShortText -> ShortText -> [Int]
-- indicesBrutal needle haystack =
--     GHC.build $ \ cons nil -> go cons nil 0
--   where
--     !l_needle = lengthWord8 needle
--     !l_haystack = lengthWord8 haystack
--     go cons nil !i
--         | i < l_haystack
--         = case compareWord8Slices needle 0 haystack i l_needle of
--             EQ -> i `cons` go cons nil (i + 1)
--             _  -> go cons nil (i + 1)
--         | otherwise
--         = nil
-- {-# INLINABLE indicesBrutal #-}

compareWord8Slices ::
    ShortText -> Int -> ShortText -> Int -> Int -> Ordering
compareWord8Slices
    (SBS ( IBS.SBS x )) ( I# off_x )
    (SBS ( IBS.SBS y )) ( I# off_y )
    ( I# n )
    = case GHC.compareByteArrays# x off_x y off_y n of
        sign
            -- Matching the default 'compare' definition lets GHC make slightly better code for the *fixOf functions (and, in fact, Eq -- so let's do that.).
            -- There are "clever" nonbranching things we could do here, but GHC seems to do a better job with explicit branches that it can eliminate.
            | isTrue# ( sign ==# 0# ) -> EQ
            | isTrue# ( sign <# 0# )  -> LT  -- could test with <=#
            | otherwise               -> GT
{-# INLINE compareWord8Slices #-}

sliceWord8 :: Int -> Int -> ShortText -> ShortText
{-^ @sliceWord8 offset size txt@ creates a new @ShortText@ that consists of @size@ bytes of @txt@, starting at byte offset @offset@.

WARNING: This can create invalid (non-UTF-8) 'ShortText' values, and the result is unspecified if @'lengthWord8' txt@ < @offset + size@, or if @offset@ or @size@ is negative. Use with extreme caution.
-}
sliceWord8 ( I# off# ) ( I# n# ) txt@(SBS ba@( IBS.SBS ba# ))
    = GHC.runRW# $ \ s0 ->
        case GHC.newByteArray# n# s0 of
            (# s1, mba# #) ->
                case GHC.copyByteArray# ba# off# mba# 0# n# s1 of
                    s2 ->
                        case GHC.unsafeFreezeByteArray# mba# s2 of
                            (# _, ba'# #) ->
                                SBS ( IBS.SBS ba'# )

replicate :: Int -> ShortText -> ShortText
{-^ @replicate n txt@ O(n * length txt)

Duplicate the 'Text' @n@ times. Please not that this is not equivalent to 'Data.List.replicate'.

prop> \ n txt -> replicate n txt == concat (List.repeat n txt)

prop> \ n txt -> replicate n txt == mtimesDefault n txt

If @n@ = 1, @replicate n txt@ is @txt@; if @n@ = 0, it is 'empty'; if @n@ < 0 it throws an error. Otherwise, it produces a completely new 'ShortText'.
-}
replicate n@( I# n# ) txt@(SBS ( IBS.SBS ba# ))
    | n == 1
    = txt
    | n < 0
    -- Data.Text.replicate is 'empty' for negative multipliers.
    = errorWithoutStackTrace "Data.ByteString.Text.Short.replicate: negative multiplier"
    | isTrue# ( len'# ==# 0# )
    = empty
    | isTrue# ( len'# <# 0# )  -- Multiplication overflowed to negative.
    = errorWithoutStackTrace "Data.ByteString.Text.Short.Replicate: overflow"
    | otherwise
    = runRW# $ \ s0 ->
        case newByteArray# len'# s0 of
            (# s1, mba# #) ->
              case replicate_loop mba# s1 0# of
                  s2 ->
                      case unsafeFreezeByteArray# mba# s2 of
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


instance GHC.IsString ShortText where
    fromString = pack
    {-# INLINE fromString #-}

pack :: [Char] -> ShortText
pack = fromBuilder . Builder.fromString
{-# INLINE [~0] pack #-}

-- packN :: Int -> [Char] -> ShortText
-- packN n = fromBuilderWith (n * 4) . Builder.fromString

fromBuilder :: Builder.Builder -> ShortText
fromBuilder =
    unsafeFromLazyByteString . BSB.toLazyByteString . Builder.toByteStringBuilder
{-# INLINE fromBuilder #-}

-- fromBuilderWith :: Int -> Builder.Builder -> ShortText
-- fromBuilderWith n =
--     unsafeFromLazyByteString . BSB.toLazyByteStringWith n . Builder.toByteStringBuilder
-- {-# INLINE fromBuilderith #-}

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
