{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module Test.Immutaly.Internal.StreamChunk.Builder (specs) where

import           Data.Extensible
import           Data.Immutaly.Internal.StreamChunk
import           Data.Immutaly.Internal.StreamChunk.Builder
import           Test.Hspec

spec_Chunk :: Spec
spec_Chunk =
  describe "embedding a single chunk works correctly" $ do
    it "toStreamChunks (chunk []) Beginning = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[]] <: nil, Continuation)" $
      toStreamChunks (chunk ([] :: [Int])) Beginning `shouldBe`
        (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[]] <: nil, Continuation)
    it "toStreamChunks (chunk []) Continuation = (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[]] <: nil, Continuation)" $
      toStreamChunks (chunk ([] :: [Int])) Continuation `shouldBe`
        (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[]] <: nil, Continuation)
    it "toStreamChunks (chunk [1,2,3]) Beginning = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)" $
      toStreamChunks (chunk ([1,2,3] :: [Int])) Beginning `shouldBe`
        (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)
    it "toStreamChunks (chunk [1,2,3]) Continuation = (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)" $
      toStreamChunks (chunk ([1,2,3] :: [Int])) Continuation `shouldBe`
        (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)

spec_Mempty :: Spec
spec_Mempty =
  describe "mempty behaves correctly" $ do
    it "toStreamChunks mempty Beginning = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= ([] :: [[Int]]) <: nil, Continuation)" $
      toStreamChunks mempty Beginning `shouldBe`
        (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= ([] :: [[Int]]) <: nil, Continuation)
    it "toStreamChunks mempty Continuation = (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= ([] :: [[Int]]) <: nil, Continuation)" $
      toStreamChunks mempty Continuation `shouldBe`
        (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= ([] :: [[Int]]) <: nil, Continuation)
    it "toStreamChunks (mempty <> chunk [1,2,3]) Beginning = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)" $
      toStreamChunks (mempty <> chunk [1,2,3]) Beginning `shouldBe`
        (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)
    it "toStreamChunks (mempty <> chunk [1,2,3]) Continuation = (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)" $
      toStreamChunks (mempty <> chunk [1,2,3]) Continuation `shouldBe`
        (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)
    it "toStreamChunks (chunk [1,2,3] <> mempty) Beginning = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)" $
      toStreamChunks (chunk [1,2,3] <> mempty) Beginning `shouldBe`
        (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)
    it "toStreamChunks (chunk [1,2,3] <> mempty) Continuation = (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)" $
      toStreamChunks (chunk [1,2,3] <> mempty) Continuation `shouldBe`
        (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3]] <: nil, Continuation)

spec_Delim :: Spec
spec_Delim =
  describe "delmitting a stream works correctly" $ do
    it "toStreamChunks delim Beginning = (#start @= Beginning <: #end @= End <: #chunks @= ([[]] :: [[Int]]) <: nil, Beginning)" $
      toStreamChunks delim Beginning `shouldBe`
        (#start @= Beginning <: #end @= End <: #chunks @= ([[]] :: [[Int]]) <: nil, Beginning)
    it "toStreamChunks delim Continuation = (#start @= Continuation <: #end @= End <: #chunks @= ([[]] :: [[Int]]) <: nil, Beginning)" $
      toStreamChunks delim Continuation `shouldBe`
        (#start @= Continuation <: #end @= End <: #chunks @= ([[]] :: [[Int]]) <: nil, Beginning)
    it "toStreamChunks (chunk [1,2,3] <> delim) Beginning = (#start @= Beginning <: #end @= End <: #chunks @= [[1,2,3]] <: nil, Beginning)" $
      toStreamChunks (chunk [1,2,3] <> delim) Beginning `shouldBe`
        (#start @= Beginning <: #end @= End <: #chunks @= [[1,2,3]] <: nil, Beginning)
    it "toStreamChunks (chunk [1,2,3] <> delim) Continuation = (#start @= Continuation <: #end @= End <: #chunks @= [[1,2,3]] <: nil, Beginning)" $
      toStreamChunks (chunk [1,2,3] <> delim) Continuation `shouldBe`
        (#start @= Continuation <: #end @= End <: #chunks @= [[1,2,3]] <: nil, Beginning)
    it "toStreamChunks (delim <> chunk [1,2,3]) Beginning = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[], [1,2,3]] <: nil, Continuation)" $
      toStreamChunks (delim <> chunk [1,2,3]) Beginning `shouldBe`
        (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[], [1,2,3]] <: nil, Continuation)
    it "toStreamChunks (delim <> chunk [1,2,3]) Continuation = (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[], [1,2,3]] <: nil, Continuation)" $
      toStreamChunks (delim <> chunk [1,2,3]) Continuation `shouldBe`
        (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[], [1,2,3]] <: nil, Continuation)

spec_Semigroup :: Spec
spec_Semigroup =
  describe "semigroup operator works as expected" $ do
    it "toStreamChunks (chunk [1,2,3] <> chunk [4,5,6]) Beginning = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3,4,5,6]] <: nil, Continuation)" $
      toStreamChunks (chunk [1,2,3] <> chunk [4,5,6]) Beginning `shouldBe`
        (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3,4,5,6]] <: nil, Continuation)
    it "toStreamChunks (chunk [1,2,3] <> chunk [4,5,6]) Continuation = #start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3,4,5,6]] <: nil, Continuation)" $
      toStreamChunks (chunk [1,2,3] <> chunk [4,5,6]) Continuation `shouldBe`
        (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3,4,5,6]] <: nil, Continuation)
    it "toStreamChunks (chunk [1,2,3] <> delim <> delim <> chunk [4,5,6]) Continuation = (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1,2,3],[],[4,5,6]] <: nil, Continuation)" $
      toStreamChunks (chunk [1,2,3] <> delim <> delim <> chunk [4,5,6]) Continuation
        `shouldBe` (#start @= Continuation <: #end @= ToBeContinued <: #chunks @= [[1,2,3],[],[4,5,6]] <: nil, Continuation)
    it "toStreamChunks (delim <> chunk [1,2,3] <> chunk [4,5,6] <> delim) Continuation = (#start @= Continuation <: #end @= End <: #chunks @= [[],[1,2,3],[4,5,6]] <: nil, Beginning)" $
      toStreamChunks (delim <> chunk [1,2,3] <> chunk [4,5,6] <> delim) Continuation
        `shouldBe` (#start @= Continuation <: #end @= End <: #chunks @= [[],[1,2,3,4,5,6]] <: nil, Beginning)
    it "toStreamChunks (delim <> delim <> delim <> delim) Beginning = (#start @= Beginning <: #end @= End <: #chunks @= ([[],[],[],[]] :: [[Int]]) <: nil, Beginning)" $
      toStreamChunks (delim <> delim <> delim <> delim) Beginning
        `shouldBe` (#start @= Beginning <: #end @= End <: #chunks @= ([[],[],[],[]] :: [[Int]]) <: nil, Beginning)

specs :: Spec
specs =
  describe "StreamChunk.Builder" $ do
    spec_Chunk
    spec_Mempty
    spec_Delim
    spec_Semigroup
