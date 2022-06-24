{-# LANGUAGE OverloadedStrings #-}
module Test.Immutaly.Internal.InMemoryImmutalyProvider.TokenSet (specs) where

import           Data.Immutaly.Internal.InMemoryImmutalyProvider.TokenSet

import           Control.Lens
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.Token
import qualified Data.Map                                                 as Map
import           Data.Time
import           Test.Hspec
import Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamReadState

specs :: Spec
specs =
  describe "Stream" $ do
    spec_ValidatePushToken
    spec_ValidateReadToken
    spec_ValidateConsumeToken
    spec_UpdatePushToken
    spec_UpdateReadToken
    spec_UpdateConsumeToken

exampleTime :: UTCTime
exampleTime = read "2011-11-19 18:28:52.607875 UTC"

exampleTime2 :: UTCTime
exampleTime2 = read "2022-11-19 18:18:51.609875 UTC"

spec_ValidatePushToken :: Spec
spec_ValidatePushToken =
  describe "a push token is valid if and only if it matches one in the token set" $ do
    it "validatePushToken fails for empty token set with stream does not exist error message" $
      validatePushToken "a" (PushToken "123") mempty `shouldBe` Left "stream a does not exist"
    it "validatePushToken fails for non-empty token set when the stream name does not match" $
      validatePushToken "a" (PushToken "123") (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "stream a does not exist"
    it "validatePushToken fails for non-empty token set when the stored token is Nothing" $
      validatePushToken "x" (PushToken "123") (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "invalid push token"
    it "validatePushToken fails for non-empty token set when the stored token does not match the supplied token" $
      validatePushToken "x" (PushToken "123") (Map.singleton "x" (newStreamTokenSet & pushToken ?~ (PushToken "456", exampleTime)))
        `shouldBe` Left "invalid push token"
    it "validatePushToken succeeds when the stored token matches the supplied token" $
      validatePushToken "x" (PushToken "123") (Map.singleton "x" (newStreamTokenSet & pushToken ?~ (PushToken "123", exampleTime)))
        `shouldBe` Right ()

spec_ValidateReadToken :: Spec
spec_ValidateReadToken =
  describe "a read token is valid if and only if it matches one in the token set" $ do
    it "validateReadToken fails for empty token set with stream does not exist error message" $
      validateReadToken "a" (ReadToken "123") mempty `shouldBe` Left "stream a does not exist"
    it "validateReadToken fails for non-empty token set when the stream name does not match" $
      validateReadToken "a" (ReadToken "123") (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "stream a does not exist"
    it "validateReadToken fails for non-empty token set when the stored token is Nothing" $
      validateReadToken "x" (ReadToken "123") (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "invalid read token"
    it "validateReadToken fails for non-empty token set when the stored token does not match the supplied token" $
      validateReadToken "x" (ReadToken "123")
        (Map.singleton "x" (newStreamTokenSet & readTokens . at (ReadToken "456") ?~ (newStreamReadState, exampleTime)))
        `shouldBe` Left "invalid read token"
    it "validateReadToken succeeds when the stored token matches the supplied token" $
      validateReadToken "x" (ReadToken "123")
        (Map.singleton "x" (newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState & byteOffset .~ 69, exampleTime)))
        `shouldBe` Right (newStreamReadState & byteOffset .~ 69)

spec_ValidateConsumeToken :: Spec
spec_ValidateConsumeToken =
  describe "a consume token is valid if and only if it matches one in the token set" $ do
    it "validateConsumeToken fails for empty token set with stream does not exist error message" $
      validateConsumeToken "a" (ConsumeToken "123") mempty `shouldBe` Left "stream a does not exist"
    it "validateConsumeToken fails for non-empty token set when the stream name does not match" $
      validateConsumeToken "a" (ConsumeToken "123") (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "stream a does not exist"
    it "validateConsumeToken fails for non-empty token set when the stored token is Nothing" $
      validateConsumeToken "x" (ConsumeToken "123") (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "invalid consume token"
    it "validateConsumeToken fails for non-empty token set when the stored token does not match the supplied token" $
      validateConsumeToken "x" (ConsumeToken "123") (Map.singleton "x" (newStreamTokenSet & consumeToken ?~ (ConsumeToken "456", exampleTime)))
        `shouldBe` Left "invalid consume token"
    it "validateConsumeToken succeeds when the stored token matches the supplied token" $
      validateConsumeToken "x" (ConsumeToken "123") (Map.singleton "x" (newStreamTokenSet & consumeToken ?~ (ConsumeToken "123", exampleTime)))
        `shouldBe` Right ()

spec_UpdatePushToken :: Spec
spec_UpdatePushToken =
  describe "updating push token works as long as stream exists and results in updated token set" $ do
    it "updatePushToken fails for empty token set with stream does not exist error message" $
      updatePushToken "a" (PushToken "123") exampleTime mempty `shouldBe` Left "stream a does not exist"
    it "updatePushToken fails for non-empty token set when the stream name does not match" $
      updatePushToken "a" (PushToken "123") exampleTime (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "stream a does not exist"
    it "updatePushToken succeeds for non-empty token set when the stream name matches" $
      updatePushToken "a" (PushToken "123") exampleTime (Map.singleton "a" newStreamTokenSet)
        `shouldBe` Right (Map.singleton "a" (newStreamTokenSet & pushToken ?~ (PushToken "123", exampleTime)))
    it "updatePushToken successfully overwrites old token" $
      (updatePushToken "a" (PushToken "456") exampleTime2 (Map.singleton "a" newStreamTokenSet)
        >>= updatePushToken "a" (PushToken "123") exampleTime)
        `shouldBe` Right (Map.singleton "a" (newStreamTokenSet & pushToken ?~ (PushToken "123", exampleTime)))

spec_UpdateReadToken :: Spec
spec_UpdateReadToken =
  describe "updating read token works as long as stream exists and results in updated token set" $ do
    it "updateReadToken fails for empty token set with stream does not exist error message" $
      updateReadToken "a" Nothing Nothing mempty `shouldBe` Left "stream a does not exist"
    it "updateReadToken fails for non-empty token set when the stream name does not match" $
      updateReadToken "a" Nothing Nothing
        (Map.singleton "x" $ newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState, exampleTime))
          `shouldBe` Left "stream a does not exist"
    it "updateReadToken noop succeeds for non-empty token set when the stream name matches" $
      updateReadToken "a" Nothing Nothing
        (Map.singleton "a" $ newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState, exampleTime))
          `shouldBe` Right (Map.singleton "a" $ newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState, exampleTime))
    it "updateReadToken delete succeeds even when token to delete does not exist" $
      updateReadToken "a" (Just $ ReadToken "123") Nothing (Map.singleton "a" newStreamTokenSet) `shouldBe` Right (Map.singleton "a" newStreamTokenSet)
    it "updateReadToken delete succeeds when token to delete exists" $
      updateReadToken "a" (Just $ ReadToken "123") Nothing
        (Map.singleton "a" $ newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState, exampleTime))
          `shouldBe` Right (Map.singleton "a" newStreamTokenSet)
    it "updateReadToken insert succeeds when the stream name matches" $
      updateReadToken "a" (Just $ ReadToken "123") (Just (ReadToken "456", newStreamReadState, exampleTime2))
        (Map.singleton "a" $ newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState, exampleTime))
          `shouldBe` Right (Map.singleton "a" $ newStreamTokenSet & readTokens . at (ReadToken "456") ?~ (newStreamReadState, exampleTime2))
    it "updateReadToken inserting and deleting the same token retains the token" $
      updateReadToken "a" (Just $ ReadToken "123") (Just (ReadToken "123", newStreamReadState, exampleTime2))
        (Map.singleton "a" $ newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState, exampleTime))
          `shouldBe` Right (Map.singleton "a" $ newStreamTokenSet & readTokens . at (ReadToken "123") ?~ (newStreamReadState, exampleTime2))

spec_UpdateConsumeToken :: Spec
spec_UpdateConsumeToken =
  describe "updating consume token works as long as stream exists and results in updated token set" $ do
    it "updateConsumeToken fails for empty token set with stream does not exist error message" $
      updateConsumeToken "a" (Just (ConsumeToken "123", exampleTime)) mempty `shouldBe` Left "stream a does not exist"
    it "updateConsumeToken fails for non-empty token set when the stream name does not match " $
      updateConsumeToken "a" (Just (ConsumeToken "123", exampleTime)) (Map.singleton "x" newStreamTokenSet) `shouldBe` Left "stream a does not exist"
    it "updateConsumeToken succeeds for non-empty token set when the stream name matches" $
      updateConsumeToken "a" (Just (ConsumeToken "123", exampleTime)) (Map.singleton "a" newStreamTokenSet)
        `shouldBe` Right (Map.singleton "a" (newStreamTokenSet & consumeToken ?~ (ConsumeToken "123", exampleTime)))
    it "updateConsumeToken successfully overwrites old token" $
      (updateConsumeToken "a" (Just (ConsumeToken "456", exampleTime2)) (Map.singleton "a" newStreamTokenSet)
        >>= updateConsumeToken "a" (Just (ConsumeToken "123", exampleTime)))
        `shouldBe` Right (Map.singleton "a" (newStreamTokenSet & consumeToken ?~ (ConsumeToken "123", exampleTime)))
    it "updateConsumeToken deletes old token when supplied token is Nothing" $
      (updateConsumeToken "a" (Just (ConsumeToken "456", exampleTime2)) (Map.singleton "a" newStreamTokenSet)
        >>= updateConsumeToken "a" Nothing)
        `shouldBe` Right (Map.singleton "a" newStreamTokenSet)
