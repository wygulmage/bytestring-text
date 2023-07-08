module Main (main) where

import Prelude hiding
    (concatMap, drop, dropWhile, elem, length, map, maximum, minimum, null, reverse, singleton, span, splitAt, take, takeWhile)
import Data.ByteString.Text.Core
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary (..))
import GHC.Exts (fromList, toList)
import qualified Data.List as List

instance Arbitrary Text where
   arbitrary = fmap fromList arbitrary
   shrink = fmap fromList . shrink . toList

main :: IO ()
main = defaultMain props

props = testGroup "All Properties" $
    testProperty "pack . unpack = id" prop_pack_unpack :
    testProperty "concatMap singleton = id" prop_concatMap_singleton :
    props_take_drop :
    props_takeWhile_dropWhile :
    testProperty "reverse" prop_reverse_reverse :
    testProperty "copy" prop_copy :
    testProperty "compareLength = compare . length"
        prop_compareLength :
    props_is_fixOf :
    -- testProperty "map id = id"
    --     prop_map_id :
    []

prop_pack_unpack cs = cs == pack (unpack cs)

prop_concatMap_singleton cs = concatMap singleton cs == cs

props_take_drop = testGroup "drop, dropEnd, take, takeEnd" $
    testProperty "drop 0 = id"
        prop_drop_0 :
    testProperty "0 <= n ==> length (drop n cs) == max 0 (length cs - n)"
        prop_drop_length :
    testProperty "dropEnd 0 = id"
        prop_dropEnd_0 :
    testProperty "0 <= n ==> length (dropEnd n cs) == max 0 (length cs - n)"
        prop_dropEnd_length :
    testProperty "take (length cs) cs = cs"
        prop_take_all :
    testProperty "0 <= n ==> length (take n cs) == min n (length cs)"
        prop_take_length :
    testProperty "takeEnd (length cs) cs = cs"
        prop_takeEnd_all :
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

prop_reverse_reverse cs = cs == reverse (reverse cs)

prop_reverse_length cs = length cs == length (reverse cs)


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

-- prop_map_id cs = map id cs == cs

-- props_list_equivalent_summaries =
--     testGroup "summary = List.summary . toList" $
--         testProperty "maximum" prop_list_maximum :
--         testProperty "minimum" prop_list_minimum :
--         testProperty "elem" prop_list_elem :
--         []

-- prop_list_maximum cs = maximum cs == List.maximum (toList cs)
-- prop_list_minimum cs = minimum cs == List.minimum (toList cs)
-- prop_list_elem c cs = elem c cs == List.elem c (toList cs)
