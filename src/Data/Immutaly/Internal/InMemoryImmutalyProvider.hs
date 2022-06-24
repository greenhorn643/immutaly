{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Immutaly.Internal.InMemoryImmutalyProvider where

import           Control.Arrow
import           Control.Lens                                                    hiding
                                                                                 ((:>))
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.ByteString.Builder                                         as B
import           Data.ByteString.Lazy                                            (ByteString)
import qualified Data.ByteString.Lazy                                            as LB
import           Data.Coerce
import           Data.Extensible
import           Data.Extensible.StreamFmt.Schema
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.DataSet
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.SchemaRegister
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamReadState
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.Token
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.TokenSet
import           Data.Immutaly.Internal.StreamChunk
import           Data.Immutaly.Internal.Types                                    hiding
                                                                                 (End)
import           Data.Immutaly.Internal.Util
import           Data.Immutaly.Internal.Util.Lens
import           Data.Int
import qualified Data.List                                                       as List
import           Data.Maybe                                                      (isNothing)
import           Data.Serialize                                                  hiding
                                                                                 (get,
                                                                                  put)
import qualified Data.Serialize                                                  as S
import           Data.Text                                                       (Text)
import qualified Data.Text                                                       as T
import           Data.Time
import           Debug.Trace
import           Fmt
import           GHC.TypeLits
import           System.Random

type InMemoryImmutalyProvider = Record
  [ "tokenSet"        :> TokenSet
  , "schemaRegister"  :> SchemaRegister
  , "dataSet"         :> DataSet
  , "gen"             :> StdGen
  ]

newIMIP :: InMemoryImmutalyProvider
newIMIP =
     #tokenSet        @= mempty
  <: #schemaRegister  @= mempty
  <: #dataSet         @= mempty
  <: #gen             @= mkStdGen 1337
  <: nil

mkField "tokenSet schemaRegister dataSet gen"

type IMIPState a = StateT InMemoryImmutalyProvider (Either String) a

verifyStreamSchema ::
  forall a. Schema a
  => Text
  -> Proxy a
  -> InMemoryImmutalyProvider
  -> Either String ()
verifyStreamSchema name _ p = do
  p ^. schemaRegister & validateSchema name (schemaHash @a)

reifyStreamSchema ::
  forall a. Schema a
  => Text
  -> Proxy a
  -> InMemoryImmutalyProvider
  -> Either String InMemoryImmutalyProvider
reifyStreamSchema name proxy p = do
  let p' = p & schemaRegister %~ addStreamSchemaIfNotExists name (schemaEntry @a)
  verifyStreamSchema name proxy p'
  return p' <&> (dataSet %~ addDataStreamIfNotExists name) . (tokenSet %~ addStreamIfNotExists name)

verifySchemaIMIP ::
  forall xs. Forall (KeyTargetAre KnownSymbol Schema) xs
  => Proxy xs -> IMIPState ()
verifySchemaIMIP _ = do
  p <- get
  lift $ henumerateFor
    (Proxy @(KeyTargetAre KnownSymbol Schema))
    (Proxy @xs)
    (\m prev -> do
      prev
      let name = T.pack $ symbolVal (proxyKeyOf m)
      verifyStreamSchema name (proxyTargetOf m) p)
    (pure ())

reifySchemaIMIP ::
  forall xs. Forall (KeyTargetAre KnownSymbol Schema) xs
  => Proxy xs -> IMIPState ()
reifySchemaIMIP _ = modifyM $ henumerateFor
  (Proxy @(KeyTargetAre KnownSymbol Schema))
  (Proxy @xs)
  (\m p -> p >>= reifyStreamSchema
    (T.pack $ symbolVal (proxyKeyOf m))
    (proxyTargetOf m))
  . pure

encodeLazyMany :: (Serialize a, Foldable t) => t a -> ByteString
encodeLazyMany = B.toLazyByteString . foldMap (B.byteString . encode)

decodeManyLazy :: Serialize a => ByteString -> Either String ([a], ByteString)
decodeManyLazy bs = trace ("decodeManyLazy " <> show bs <> " = " <> show (snd <$> result)) result
  where result = case runGetLazyState S.get bs of
          Left _         -> Right ([], bs)
          Right (a, bs') -> first (a :) <$> decodeManyLazy bs'

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _ = pure ()
whenJust (Just a) f = f a

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing Nothing f = f
whenNothing _       _ = pure ()

generateToken :: IMIPState TokenFragment
generateToken = gen %%= nextTokenFragment

processPushTokenIMIP :: Text -> Maybe PushToken -> UTCTime -> IMIPState PushToken
processPushTokenIMIP name pt ts = verifyToken >> (coerce <$> generateToken) >>= updateToken
  where verifyToken     = get >>= lift . whenJust pt . flip (validatePushToken name) . (^. tokenSet)
        updateToken pt  = tokenSet %%%= updatePushToken name pt ts >> pure pt

writeToStreamIMIP :: (Serialize a, Foldable t) => Text -> t a -> IMIPState ()
writeToStreamIMIP name as = do
  dataSet %%%= writeToStream name (encodeLazyMany as)

pushToStreamIMIP ::
  forall a t. (Schema a, Serialize a, Foldable t)
  => Text
  -> t a
  -> Maybe PushToken
  -> UTCTime
  -> IMIPState PushToken
pushToStreamIMIP name as pt ts = do
  get >>= lift . verifyStreamSchema name (Proxy @a)
  whenNothing pt $ dataSet %%%= startNewStreamFragment name
  pt <- processPushTokenIMIP name pt ts
  writeToStreamIMIP name as
  return pt

decodeStreamChunks :: Serialize a => StreamChunks ByteString -> Either String (StreamChunks [a], ByteString)
decodeStreamChunks sc = do
  (scs, extra) <- go (sc ^. chunks)
  let sc' = #start @= sc ^. start
        <:  #end   @= sc ^. end
        <:  #chunks @= scs
        <:  nil
  return (sc', extra)
  where go []       = pure ([],"")
        go [ch]     = first List.singleton <$> decodeManyLazy ch
        go (ch:chs) = do (sc, extra) <- decodeManyLazy ch
                         when (extra /= "") $ Left "error decoding stream chunks: unexpected unparsed bytes"
                         (scs', extra') <- go chs
                         return (sc:scs', extra')

processReadIMIP :: Serialize a => Int64 -> Text -> StreamReadState -> IMIPState (StreamChunks [a], Maybe StreamReadState)
processReadIMIP maxBytes name readState = do
  (rawData, readState') <- dataSet %%> readFromDataSet maxBytes readState name
  (as, extraBytes) <- lift $ decodeStreamChunks rawData
  case (extraBytes, readState') of
    ("", rs)      -> return (as, rs)
    (bs, Just rs) -> do rs' <- dataSet %%> rewindReadStateOnDataSet (LB.length extraBytes) (readState' ^?! _Just) name
                        if rs' ^. byteOffset == 0
                          then return (as & end .~ End & chunks %~ init, Just rs')
                          else return (as, Just rs')
    _             -> lift $ Left "error decoding read stream: unexpected bytes"

validateReadTokenIMIP :: Text -> ReadToken -> IMIPState StreamReadState
validateReadTokenIMIP name rt = do
  p <- get
  lift $ validateReadToken name rt (p ^. tokenSet)

updateReadTokenIMIP :: Text -> Maybe ReadToken -> Maybe (ReadToken, StreamReadState, UTCTime) -> IMIPState ()
updateReadTokenIMIP name oldToken newTokenInfo =
  tokenSet %%%= updateReadToken name oldToken newTokenInfo

readFromStreamIMIP ::
  forall a. (Schema a, Serialize a)
  => Int64
  -> Text
  -> Maybe ReadToken
  -> UTCTime
  -> IMIPState (StreamChunks [a], Maybe ReadToken)
readFromStreamIMIP maxBytes name rt ts = do
  get >>= lift . verifyStreamSchema name (Proxy @a)
  rs <- maybe (pure newStreamReadState) (validateReadTokenIMIP name) rt
  (as, rs') <- processReadIMIP maxBytes name rs
  case rs' of
    Just rs'  ->  do rt' <- coerce <$> generateToken
                     updateReadTokenIMIP name rt (Just (rt', rs', ts))
                     return (as, Just rt')
    Nothing   ->  do updateReadTokenIMIP name rt Nothing
                     return (as, Nothing)