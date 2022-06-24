{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}

module Test.Immutaly.Internal.Util.Composite (specs) where

import           Data.Immutaly.Internal.Util.Composite
import           Test.Hspec

spec_ToFromList :: Spec
spec_ToFromList =
  describe "toList inverts fromList" $ do
    it "toList . fromList $ [] = []" $
      (toList . fromList) ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "toList . fromList $ [1] = [1]" $
      (toList . fromList) ([1] :: [Int]) `shouldBe` ([1] :: [Int])
    it "toList . fromList $ [1..100] = [1..100]" $
      (toList . fromList) ([1..100] :: [Int]) `shouldBe` ([1..100] :: [Int])

spec_Item :: Spec
spec_Item =
  describe "item embeds an item of type a in a Composite a" $ do
    it "toList $ item 37 = [37]" $
      toList (item 37) `shouldBe` [37]

spec_Mempty :: Spec
spec_Mempty =
  describe "mempty embeds the empty list" $ do
    it "toList mempty = []" $
      toList mempty `shouldBe` ([] :: [Int])
    it "toList $ mempty <> item 42 = [42]" $
      toList (mempty <> item 42) `shouldBe` [42]
    it "toList $ item 42 <> mempty = [42]" $
      toList (item 42 <> mempty) `shouldBe` [42]

spec_Semigroup :: Spec
spec_Semigroup =
  describe "toList $ comp1 <> comp2 = toList comp1 <> toList comp2" $ do
    it "toList $ item 1 <> item 2 <> item 3 = [1,2,3]" $
      toList (item 1 <> item 2 <> item 3) `shouldBe` [1,2,3]
    it "toList $ item 1 <> fromList [2, 3, 4] <> item 5 <> item 6 <> fromList [7, 8] <> item 9 = [1..9]" $
      toList (item 1 <> fromList [2, 3, 4] <> item 5 <> item 6 <> fromList [7, 8] <> item 9) `shouldBe` [1..9]

specs :: Spec
specs =
  describe "Composite" $ do
    spec_ToFromList
    spec_Item
    spec_Mempty
    spec_Semigroup
