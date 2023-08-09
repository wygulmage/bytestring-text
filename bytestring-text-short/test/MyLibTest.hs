module Main (main) where

import Data.ByteString.Text.Short
import Data.ByteString.Text.Short.Internal
import Data.ByteString.Text.Short.Internal.Search

import qualified Data.ByteString.Short as BS

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Function (apply)
-- import Test.QuickCheck.Unicode (fromUnicode)

instance Arbitrary ShortText where
   arbitrary = fmap pack arbitrary
   shrink = fmap pack . shrink . unpack

main :: IO ()
main = defaultMain $
    testGroup "indices" $
        testProperty "brute force and two-way are equivalent"
            prop_indices_equivalent :
--        testProperty "critical period is doubled when the string is doubled"
--            prop_critical_period_doubled :
        []

prop_indices_equivalent :: ShortText -> ShortText -> Bool
prop_indices_equivalent (SBS needle) (SBS haystack) =
    BS.null needle
    || indicesBrutalBS needle haystack == two_Way_Pattern_Matching needle haystack

{-
prop_critical_period_doubled :: ShortText -> Bool
prop_critical_period_doubled (SBS bs) =
    BS.null bs
    || case critFact (bs <> bs) of (_, period) -> even period && period > 0
-}
