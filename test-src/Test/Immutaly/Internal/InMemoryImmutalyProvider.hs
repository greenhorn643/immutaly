{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Immutaly.Internal.InMemoryImmutalyProvider (specs) where

import           Control.Lens                                    hiding ((:>))
import           Control.Monad.Trans.State.Strict
import           Data.Extensible
import           Data.Immutaly.Internal.InMemoryImmutalyProvider
import           Data.Maybe
import           Data.Time
import           Data.Typeable
import           Test.Hspec
import Data.Immutaly.Internal.StreamChunk

type Schema1 =
  [ "x" :> Double
  , "y" :> Int
  , "z" :> Bool
  ]

exampleTime1 :: UTCTime
exampleTime1 = read "2011-11-19 18:28:52.607875 UTC"

exampleTime2 :: UTCTime
exampleTime2 = read "2011-11-19 18:28:53.607875 UTC"

exampleTime3 :: UTCTime
exampleTime3 = read "2011-11-19 18:28:54.607875 UTC"

spec_VerifySchemaSucceeds :: Spec
spec_VerifySchemaSucceeds =
  describe "verifySchema succeeds when the db schema matches the requested schema" $ do
    it "succeeds for the empty schema" $
      evalStateT (verifySchemaIMIP $ Proxy @'[]) newIMIP `shouldBe` pure ()
    it "succeeds for reified schema" $
      evalStateT (reifySchemaIMIP (Proxy @Schema1) >> verifySchemaIMIP (Proxy @Schema1)) newIMIP `shouldBe` pure ()


spec_PushToStreamSucceeds :: Spec
spec_PushToStreamSucceeds = do
  describe "pushToStream succeeds" $ do
    it "succeeds for reified schema with valid tokens" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) (Just pt1) exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) (Just pt2) exampleTime3
        return ()) newIMIP
      `shouldBe` Right ()
    it "readback succeeds for reified schema with valid tokens" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) (Just pt1) exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) (Just pt2) exampleTime3
        (as, rt) <- readFromStreamIMIP 100 "x" Nothing exampleTime1
        return (as, rt)) newIMIP
      `shouldBe` Right (#start @= Beginning <: #end @= End <: #chunks @= [[1,2,3] :: [Double]] <: nil, Nothing)
    it "readback produces many chunks for Nothing tokens" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) Nothing exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) Nothing exampleTime3
        (as, rt) <- readFromStreamIMIP 100 "x" Nothing exampleTime1
        return (as, rt)) newIMIP
      `shouldBe` Right (#start @= Beginning <: #end @= End <: #chunks @= [[1] :: [Double], [2], [3]] <: nil, Nothing)
    it "partial readback returns non-Nothing read token | continuous stream" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) (Just pt1) exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) (Just pt2) exampleTime3
        pt4 <- pushToStreamIMIP "x" ([4.0] :: [Double]) (Just pt3) exampleTime3
        (as, rt) <- readFromStreamIMIP 18 "x" Nothing exampleTime1
        return (as, isJust rt)) newIMIP
      `shouldBe` Right (#start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1 :: Double, 2]] <: nil, True)
    it "partial readback returns non-Nothing read token | fragmented streams 1" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) (Just pt1) exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) Nothing exampleTime3
        pt4 <- pushToStreamIMIP "x" ([4.0] :: [Double]) (Just pt3) exampleTime3
        (as, rt) <- readFromStreamIMIP 16 "x" Nothing exampleTime1
        return (as, isJust rt)) newIMIP
      `shouldBe` Right (#start @= Beginning <: #end @= End <: #chunks @= [[1 :: Double, 2]] <: nil, True)
    it "partial readback returns non-Nothing read token | fragmented streams 2" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) Nothing exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) Nothing exampleTime3
        pt4 <- pushToStreamIMIP "x" ([4.0] :: [Double]) Nothing exampleTime3
        (as, rt) <- readFromStreamIMIP 18 "x" Nothing exampleTime1
        return (as, isJust rt)) newIMIP
      `shouldBe` Right (#start @= Beginning <: #end @= End <: #chunks @= [[1 :: Double], [2]] <: nil, True)
    it "makes successful successive reads | continuous stream" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) (Just pt1) exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) (Just pt2) exampleTime3
        pt4 <- pushToStreamIMIP "x" ([4.0] :: [Double]) (Just pt3) exampleTime3
        (as1, rt1) <- readFromStreamIMIP 18 "x" Nothing exampleTime1
        (as2, rt2) <- readFromStreamIMIP 18 "x" rt1 exampleTime2
        return (as1, as2, isJust rt2)) newIMIP
      `shouldBe` Right (
        #start @= Beginning <: #end @= ToBeContinued <: #chunks @= [[1 :: Double, 2]] <: nil,
        #start @= Continuation <: #end @= End <: #chunks @= [[3 :: Double, 4]] <: nil,
        False )
    it "makes successful successive reads | fragmented streams 1" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) (Just pt1) exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) Nothing exampleTime3
        pt4 <- pushToStreamIMIP "x" ([4.0] :: [Double]) (Just pt3) exampleTime3
        (as1, rt1) <- readFromStreamIMIP 18 "x" Nothing exampleTime1
        (as2, rt2) <- readFromStreamIMIP 18 "x" rt1 exampleTime2
        return (as1, as2, isJust rt2)) newIMIP
      `shouldBe` Right (
        #start @= Beginning <: #end @= End <: #chunks @= [[1 :: Double, 2]] <: nil,
        #start @= Beginning <: #end @= End <: #chunks @= [[3 :: Double, 4]] <: nil,
        False )
    it "makes successful successive reads | fragmented streams 2" $
      evalStateT (do
        reifySchemaIMIP (Proxy @Schema1)
        pt1 <- pushToStreamIMIP "x" ([1.0] :: [Double]) Nothing exampleTime1
        pt2 <- pushToStreamIMIP "x" ([2.0] :: [Double]) Nothing exampleTime2
        pt3 <- pushToStreamIMIP "x" ([3.0] :: [Double]) Nothing exampleTime3
        pt4 <- pushToStreamIMIP "x" ([4.0] :: [Double]) Nothing exampleTime3
        (as1, rt1) <- readFromStreamIMIP 18 "x" Nothing exampleTime1
        (as2, rt2) <- readFromStreamIMIP 18 "x" rt1 exampleTime2
        return (as1, as2, isJust rt2)) newIMIP
      `shouldBe` Right (
        #start @= Beginning <: #end @= End <: #chunks @= [[1 :: Double], [2]] <: nil,
        #start @= Beginning <: #end @= End <: #chunks @= [[3 :: Double], [4]] <: nil,
        False )

specs :: Spec
specs =
  describe "InMemoryImmutalyProvider" $ do
    spec_VerifySchemaSucceeds
    spec_PushToStreamSucceeds
