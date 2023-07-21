module Main (main) where

import Prelude hiding
    (concat, concatMap, drop, dropWhile, elem, filter, head, init, last, length, map, maximum, minimum, null, reverse, singleton, span, splitAt, tail, take, takeWhile)
import Data.ByteString.Text.Char
import Data.ByteString.Text.Core
import Data.ByteString.Text.Core.Internal hiding (fromString)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Unicode (fromUnicode)
import GHC.Exts (fromString, fromList, toList)
import qualified Data.List as List
import qualified Data.ByteString as BS
import qualified Data.Text as Other
import qualified Data.Text.Encoding as Other
import qualified Data.Text.Encoding.Error as Other

instance Arbitrary Text where
   arbitrary = fmap fromList arbitrary
   shrink = fmap fromList . shrink . toList

newtype BS = BS BS.ByteString
  deriving (Show)
getBS :: BS -> BS.ByteString
getBS (BS bs) = bs

instance Arbitrary BS where
    arbitrary = fmap (BS . BS.pack) arbitrary
    shrink = fmap (BS . BS.pack) . shrink . BS.unpack . getBS

main :: IO ()
main = defaultMain props


props = testGroup "All Properties" $
    props_List :
    props_other_text :
    testProperty "decodeUtf8Lenient produces valid UTF-8"
        prop_isValidUtf8_decodeUtf8Lenient :
    testProperty "pack . unpack = id" prop_pack_unpack :
    testProperty "concatMap singleton = id" prop_concatMap_singleton :
    props_take_drop :
    props_takeWhile_dropWhile :
    props_reverse :
    testProperty "copy" prop_copy :
    testProperty "compareLength = compare . length"
        prop_compareLength :
    props_is_fixOf :
    testProperty "unicodeReplacementCharcacter = '\\xFFFD'"
        prop_fffd :
    testProperty "map id = id"
        prop_map_id :
    []



------ Make sure that the API shared with Data.List is equivalent.

props_List = testGroup "Data.List" $
    testProperty "head" prop_List_head :
    testProperty "last" prop_List_last :
    testProperty "tail" prop_List_tail :
    testProperty "init" prop_List_init :
    testProperty "take" prop_List_take :
    testProperty "takeWhile" prop_List_takeWhile :
    testProperty "drop" prop_List_drop :
    testProperty "dropWhile" prop_List_dropWhile :
    testProperty "concat" prop_List_concat :
    testProperty "filter" prop_List_filter :
    testProperty "isPrefixOf" prop_List_isPrefixOf :
    testProperty "isSuffixOf" prop_List_isSuffixOf :
    testProperty "isInfixOf" prop_List_isInfixOf :
    testProperty "stripPrefix" prop_List_stripPrefix :
    testProperty "map" prop_List_map :
    []

evenChar c = fromEnum c `rem` 2 == 0

prop_List_head str =
    List.null str || List.head str == head (fromList str)

prop_List_last str =
    List.null str || List.last str == last (fromList str)

prop_List_tail str =
    List.null str || List.tail str == toList (tail (fromList str))

prop_List_init str =
    List.null str || List.init str == toList (init (fromList str))

prop_List_take n str = List.take n str == toList (take n (fromList str))

prop_List_takeWhile p str =
    List.takeWhile (apply p) str == toList (takeWhile (apply p) (fromList str))

prop_List_drop n str = List.drop n str == toList (drop n (fromList str))

prop_List_dropWhile p str =
    List.dropWhile (apply p) str == toList (dropWhile (apply p) (fromList str))

prop_List_concat txts =
    List.concat (fmap toList txts) == toList (concat txts)

prop_List_filter str =
    List.filter evenChar str == toList (filter evenChar (fromList str))

prop_List_isPrefixOf pre str =
    List.isPrefixOf pre str == isPrefixOf (fromList pre) (fromList str)

prop_List_isSuffixOf pre str =
    List.isSuffixOf pre str == isSuffixOf (fromList pre) (fromList str)

prop_List_isInfixOf pre str =
    List.isInfixOf pre str == isInfixOf (fromList pre) (fromList str)

prop_List_stripPrefix pre str =
    List.stripPrefix pre str
 == fmap toList (stripPrefix (fromList pre) (fromList str))

prop_List_map f str =
    pack (List.map (apply f) str)
 == map (apply f) (pack str)
-- `pack` applies applies the same replacement as `map`


------ Make sure that the API is equivalent to the Data.Text API.

props_other_text = testGroup "Data.Text" $
    testProperty "toList . fromString"
        prop_text_toList_fromString :
    testProperty "toList . decodeUtf8"
        prop_text_toList_decodeUtf8 :
    -- testProperty "toList . decodeUtf8Lenient"
    --     prop_text_toList_decodeUtf8Lenient :
    testProperty "takeEnd" prop_text_takeEnd :
    testProperty "takeWhileEnd" prop_text_takeWhileEnd :
    testProperty "dropEnd" prop_text_dropEnd :
    testProperty "dropWhileEnd" prop_text_dropWhileEnd :
    []

prop_text_toList_fromString str = toList res1 == toList res2
  where
    res1 :: Text
    res1 = fromString str
    res2 :: Other.Text
    res2 = fromString str

prop_text_toList_decodeUtf8 (BS bs) =
    not (isValidUtf8 bs) || toList res1 == toList res2
  where
    res1 :: Text
    res1 = decodeUtf8 bs
    res2 :: Other.Text
    res2 = Other.decodeUtf8 bs

-- This one will fail, because the replacement schemes are slightly different.
prop_text_toList_decodeUtf8Lenient (BS bs) =
    toList res1 == toList res2
  where
    res1 :: Text
    res1 = decodeUtf8Lenient bs
    res2 :: Other.Text
    res2 = Other.decodeUtf8With Other.lenientDecode bs

prop_text_takeEnd n str =
    toList (takeEnd n (fromList str)) == toList (Other.takeEnd n (fromList str))

prop_text_takeWhileEnd p str =
    toList (takeWhileEnd (apply p) (fromList str))
 == toList (Other.takeWhileEnd (apply p) (fromList str))

prop_text_dropEnd n str =
    toList (dropEnd n (fromList str)) == toList (Other.dropEnd n (fromList str))

prop_text_dropWhileEnd p str =
    toList (dropWhileEnd (apply p) (fromList str))
 == toList (Other.dropWhileEnd (apply p) (fromList str))


------ General Sanity Checks

prop_fffd = replacementCharacter == singleton '\xFFFD'

prop_isValidUtf8 =
    isValidUtf8 . encodeUtf8 . pack . fromUnicode

prop_isValidUtf8_decodeUtf8Lenient (BS bs) =
    isValidUtf8 (encodeUtf8 (decodeUtf8Lenient bs))

prop_pack_unpack cs = cs == pack (unpack cs)

prop_concatMap_singleton cs = concatMap singleton cs == cs


props_take_drop = testGroup "drop, dropEnd, take, takeEnd" $
    testProperty "drop 0 = id" prop_drop_0 :
    testProperty
        "n >= 0  ==>  length (drop n cs) == max 0 (length cs - n)"
        prop_drop_length :
    testProperty "dropEnd 0 = id" prop_dropEnd_0 :
    testProperty
        "n >= 0  ==>  length (dropEnd n cs) == max 0 (length cs - n)"
        prop_dropEnd_length :
    testProperty "take (length cs) cs = cs"
        prop_take_all :
    testProperty "0 <= n ==> length (take n cs) == min n (length cs)"
        prop_take_length :
    testProperty "takeEnd (length cs) cs = cs" prop_takeEnd_all :
    testProperty "0 <= n ==> length (takeEnd n cs) == min n (length cs)"
        prop_takeEnd_length :
    testProperty "splitAt n cs == (take n cs, drop n cs)"
        prop_take_drop_splitAt :
    []

prop_drop_0 cs = cs == drop 0 cs
prop_drop_length (NonNegative n) cs = length (drop n cs) == max 0 (length cs - n)

prop_take_drop_splitAt n cs = splitAt n cs == (take n cs, drop n cs)


prop_dropEnd_0 cs = cs == dropEnd 0 cs
prop_dropEnd_length (NonNegative n) cs = length (dropEnd n cs) == max 0 (length cs - n)


prop_take_all cs = cs == take (length cs) cs
prop_take_length (NonNegative n) cs = length (take n cs) == min n (length cs)


prop_takeEnd_all cs = cs == takeEnd (length cs) cs
prop_takeEnd_length (NonNegative n) cs = length (takeEnd n cs) == min n (length cs)

props_takeWhile_dropWhile = testGroup "dropWhile, takeWhile" $
    testProperty "dropWhile (const False) = id" prop_dropWhile_0 :
    testProperty "dropWhileEnd (const False) = id" prop_dropWhileEnd_0 :
    testProperty "span (const False) x = (empty, x)" prop_span_False :
    testProperty "span (const True) x = (x, empty)" prop_span_True :
    []

prop_dropWhile_0 cs = cs == dropWhile (\_-> False) cs
prop_dropWhileEnd_0 cs = cs == dropWhileEnd (\_-> False) cs

prop_takeWhile_all cs = cs == takeWhile (\_-> True) cs
prop_takeWhileEnd_all cs = cs == takeWhileEnd (\_-> True) cs

prop_span_False cs = span (\_-> False) cs == (empty, cs)
prop_span_True cs = span (\_-> True) cs == (cs, empty)

prop_compareLength cs n = compareLength cs n == compare (length cs) n

props_reverse = testGroup "reverse" $
    testProperty "reverse . reverse = id" prop_reverse_reverse :
    testProperty "length txt == length (reverse txt)" prop_reverse_length :
    testProperty "head txt == last (reverse txt)" prop_reverse_head_last :
    testProperty "last txt == head (reverse txt)" prop_reverse_last_head :
    []

prop_reverse_reverse cs = cs == reverse (reverse cs)

prop_reverse_length cs = length cs == length (reverse cs)

prop_reverse_head_last cs = null cs || head cs == last (reverse cs)
prop_reverse_last_head cs = null cs || last cs == head (reverse cs)


props_is_fixOf = testGroup "isPrefixOf, isSuffixOf, isInfixOf" $
    testProperty "x `isPrefixOf` x" prop_isPrefixOf_self :
    testProperty "empty `isPrefixOf` x" prop_isPrefixOf_empty :
    testProperty "x `isSuffixOf` x" prop_isSuffixOf_self :
    testProperty "empty `isSuffixOf` x" prop_isSuffixOf_empty :
    testProperty "x `isInfixOf` x" prop_isInfixOf_self :
    testProperty "empty `isInfixOf` x" prop_isInfixOf_empty :
    testProperty "stripPrefix drops the prefix or is Nothing"
        prop_stripPrefix_isPrefix :
    testProperty "stripSuffix dropEnds the suffix or is Nothing"
        prop_stripSuffix_isSuffix :
    []

prop_isPrefixOf_self cs = cs `isPrefixOf` cs
prop_isPrefixOf_empty cs = empty `isPrefixOf` cs
prop_isSuffixOf_self cs = cs `isSuffixOf` cs
prop_isSuffixOf_empty cs = empty `isSuffixOf` cs
prop_isInfixOf_self cs = cs `isInfixOf` cs
prop_isInfixOf_empty cs = empty `isInfixOf` cs

prop_stripPrefix_isPrefix pre cs
    | pre `isPrefixOf` cs
    = stripped == Just (drop (length pre) cs)
    | otherwise
    = stripped == Nothing
  where
    stripped = stripPrefix pre cs

prop_stripSuffix_isSuffix suf cs
    | suf `isSuffixOf` cs
    = stripped == Just (dropEnd (length suf) cs)
    | otherwise
    = stripped == Nothing
  where
    stripped = stripSuffix suf cs

prop_copy cs = cs == copy cs

prop_map_id cs = map id cs == cs
