{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib
    ( someFunc
    ) where

import           Data.ByteString                  (ByteString)
import           qualified Data.ByteString as B
import           Data.Extensible.StreamFmt.Schema
import           Data.Serialize
import           Data.Text                        (Text)
import          qualified Data.Text.Lazy as LT
import           Data.Typeable
import           GHC.Generics
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Extensible
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding ((:>))
import System.Random
import Control.Monad
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.Maybe
import Data.Bifunctor
import Data.Hashable
import Data.Coerce

newtype TokenFragment =
  TokenFragment
    { unTokenFragment :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

newtype PushToken =
  PushToken
    { unPushToken :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

newtype ViewToken =
  ViewToken
    { unViewToken :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

newtype ReadToken =
  ReadToken
    { unReadToken :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

class (Monad m) => ImmutalyProvider m p where
  createStream :: (Schema a, Serialize a) => Proxy a -> p -> Text -> m (PushToken, p)
  pushToStream :: (Schema a, Serialize a, Foldable t) => p -> PushToken -> t a -> m (PushToken, p)
  getStreamNames :: p -> m ([Text], p)
  getViewTokens :: p -> Text -> m ([ViewToken], p)
  beginReadStream :: p -> ViewToken -> m (ReadToken, p)
  readNextChunk :: (Schema a, Serialize a, Foldable t) => p -> ReadToken -> m (t a, ReadToken, p)

mkField "datumSchema datumSchemaHash rawData pushToken viewState"

data ViewState
  = HasReaders (Map TokenFragment (TokenFragment, Int))
  | HasConsumer (TokenFragment, Int)
  | None
  deriving (Generic, Eq, Show)

type StreamFragment = Record
  [ "datumSchema"       :> Text
  , "datumSchemaHash"   :> Word64
  , "rawData"           :> Seq Word8
  , "pushToken"         :> TokenFragment
  , "viewState"         :> ViewState
  ]

emptyStreamFragment :: forall a. Schema a => StreamFragment
emptyStreamFragment =
     #datumSchema       @= LT.toStrict (B.toLazyText $ schema @a)
  <: #datumSchemaHash   @= schemaHash @a
  <: #rawData           @= mempty
  <: #pushToken         @= mempty
  <: #viewState         @= None
  <: nil

schemaHash :: forall a. Schema a => Word64
schemaHash = fromIntegral . hash . LT.toStrict . B.toLazyText $ schema @a

pushToStreamFragment ::
  forall a t. (Schema a, Serialize a, Foldable t)
  => TokenFragment
  -> TokenFragment
  -> t a
  -> StreamFragment
  -> Either String StreamFragment
pushToStreamFragment token nextToken as ds = do
  when (ds ^. pushToken       /= token)         $ Left "invalid token"
  when (ds ^. datumSchemaHash /= schemaHash @a) $ Left "invalid datum schema"
  let newRawData = Seq.fromList $ foldMap (B.unpack . encode) as
  return $  ds & rawData %~ (<> newRawData)
               & pushToken .~ nextToken

mkField "streams gen"

type InMemoryImmutalyProvider = Record
  [ "streams" :> Map Text (Map TokenFragment StreamFragment)
  , "gen"     :> StdGen
  ]

newIMIP :: InMemoryImmutalyProvider
newIMIP =
     #streams @= mempty
  <: #gen @= mkStdGen 1337
  <: nil

generate :: Int -> (g -> (a, g)) -> g -> ([a], g)
generate 0 _ g = ([], g)
generate n f g = (a:as, g'')
  where (a, g')   = f g
        (as, g'') = generate (n - 1) f g'

nextTokenFragment :: RandomGen g => g -> (TokenFragment, g)
nextTokenFragment = first (TokenFragment . B.pack) . generate 32 random

addToToken :: Serialize a => a -> TokenFragment -> TokenFragment
addToToken = mappend . TokenFragment . encode

extractFromToken :: (Serialize a) => TokenFragment -> Either String (a, TokenFragment)
extractFromToken = (fmap . fmap) TokenFragment . go . unTokenFragment
  where go = flip (runGetState get) 0

addStreamName :: Text -> TokenFragment -> TokenFragment
addStreamName = addToToken

addStreamToken :: TokenFragment -> TokenFragment -> TokenFragment
addStreamToken = addToToken . unTokenFragment

addPushToken :: TokenFragment -> TokenFragment -> TokenFragment
addPushToken = addToToken . unTokenFragment

extractStreamName :: TokenFragment -> Either String (Text, TokenFragment)
extractStreamName = extractFromToken

extractStreamToken :: TokenFragment -> Either String (TokenFragment, TokenFragment)
extractStreamToken token = do
  (t, rem) <- extractFromToken token
  return (TokenFragment t, rem)

extractPushToken :: TokenFragment -> Either String (TokenFragment, TokenFragment)
extractPushToken token = do
  (t, rem) <- extractFromToken token
  return (TokenFragment t, rem)

composePushToken :: Text -> TokenFragment -> TokenFragment -> PushToken
composePushToken name streamToken pushToken = coerce $ mempty
  & addStreamName name
  & addStreamToken streamToken
  & addPushToken pushToken

decomposePushToken :: PushToken -> Either String (Text, TokenFragment, TokenFragment)
decomposePushToken token = do
  (pushToken, token') <- extractPushToken $ coerce token
  (streamToken, token'') <- extractStreamToken token'
  (streamName, token''') <- extractStreamName token''
  return (streamName, streamToken, pushToken)

composeViewToken :: Text -> TokenFragment -> ViewToken
composeViewToken name streamToken = coerce $ mempty
  & addStreamName name
  & addStreamToken streamToken

nextTokenFragmentIMIP :: InMemoryImmutalyProvider -> (TokenFragment, InMemoryImmutalyProvider)
nextTokenFragmentIMIP p = (token, p & gen .~ gen')
  where (token, gen') = nextTokenFragment (p ^. gen)

createStreamInStreamMap ::
  forall a. Schema a
  => TokenFragment
  -> Text
  -> Map Text (Map TokenFragment StreamFragment)
  -> Map Text (Map TokenFragment StreamFragment)
createStreamInStreamMap token = Map.alter $ Just . updateStreams
  where updateStreams Nothing         = Map.singleton token (emptyStreamFragment @a)
        updateStreams (Just streams)  = Map.insert token (emptyStreamFragment @a) streams

createStreamIMIP ::
  forall a. (Schema a, Serialize a)
  => Proxy a
  -> InMemoryImmutalyProvider
  -> Text
  -> Either String (PushToken, InMemoryImmutalyProvider)
createStreamIMIP _ p name = Right (token', p'')
  where (token, p') = nextTokenFragmentIMIP p
        p''         = p' & streams %~ (createStreamInStreamMap @a) token name
        token'      = composePushToken name token mempty

checkPushTokenValid :: InMemoryImmutalyProvider -> PushToken -> Either String ()
checkPushTokenValid p token = undefined

pushToStreamIMIP ::
  forall a t. (Schema a, Serialize a, Foldable t)
  => Proxy a
  -> InMemoryImmutalyProvider
  -> PushToken
  -> t a
  -> Either String (PushToken, InMemoryImmutalyProvider)
pushToStreamIMIP _ p token as = do
  (name, streamToken, pt) <- decomposePushToken token
  let (nextPt, p') = nextTokenFragmentIMIP p

  newDS <- p' ^? streams . ix name . ix streamToken
            <&> pushToStreamFragment pt nextPt as
            & fromMaybe (Left "invalid token")

  let nextToken = composePushToken name streamToken nextPt
  let p'' = p' & streams . ix name . ix streamToken .~ newDS
  return (nextToken, p'')

getStreamNamesIMIP :: InMemoryImmutalyProvider -> Either String ([Text], InMemoryImmutalyProvider)
getStreamNamesIMIP p = Right (names, p)
  where names = p ^. streams & Map.keys

getViewTokensIMIP :: InMemoryImmutalyProvider -> Text -> Either String ([ViewToken], InMemoryImmutalyProvider)
getViewTokensIMIP p name = Right (tokens, p)
  where tokens = p ^? streams . ix name
                  <&> Map.keys
                  <&> fmap (composeViewToken name)
                  & fromMaybe []

beginReadStreamIMIP :: InMemoryImmutalyProvider -> ViewToken -> Either String (ReadToken, InMemoryImmutalyProvider)
beginReadStreamIMIP p vt = undefined

someFunc :: IO ()
someFunc = do
  let p = newIMIP
  let (Right (t1, p1)) = createStreamIMIP (Proxy @Int) p "x"
  print t1
  print p1
  
  let (Right (t2, p2)) = pushToStreamIMIP (Proxy @Int) p1 t1 [1, 2, 3]
  print t2
  print p2

  let (Left err) = pushToStreamIMIP (Proxy @Float) p2 t2 [1, 2, 3]
  print err

  let (Left err) = pushToStreamIMIP (Proxy @Int) p2 t1 [1, 2, 3]
  print err

  let (Right (ss, p3)) = getStreamNamesIMIP p2
  print ss

  let (Right (vts, p4)) = getViewTokensIMIP p3 (head ss)
  print vts