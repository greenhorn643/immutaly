{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Data.Immutaly.Internal.InMemoryImmutalyProvider.TokenSet where
import           Control.Arrow                                                   ((>>>))
import           Control.Lens                                                    hiding
                                                                                 ((:>))
import           Data.Extensible
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamGuards
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamReadState
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.Token
import           Data.Immutaly.Internal.InMemoryImmutalyProvider.Util
import           Data.Int
import           Data.Map                                                        (Map)
import qualified Data.Map                                                        as Map
import           Data.Maybe
import           Data.Text                                                       (Text)
import           Data.Time
import           Fmt

type StreamTokenSet = Record
  [ "pushToken"     :> Maybe (PushToken, UTCTime)
  , "readTokens"    :> Map ReadToken (StreamReadState, UTCTime)
  , "consumeToken"  :> Maybe (ConsumeToken, UTCTime)
  ]

mkField "pushToken readTokens consumeToken"

newStreamTokenSet :: StreamTokenSet
newStreamTokenSet =
     #pushToken     @= Nothing
  <: #readTokens    @= mempty
  <: #consumeToken  @= Nothing
  <: nil

type TokenSet = Map Text StreamTokenSet

validatePushToken :: Text -> PushToken -> TokenSet -> Either String ()
validatePushToken name pt = validateGuardedStreamExists name
  ((^. pushToken) >>> fmap fst >>> assertEq "invalid push token" (Just pt))

validateReadToken :: Text -> ReadToken -> TokenSet -> Either String StreamReadState
validateReadToken name rt = validateGuardedStreamExists name
  ((^. readTokens . at rt) >>> maybe (Left "invalid read token") (Right . fst))

validateConsumeToken :: Text -> ConsumeToken -> TokenSet -> Either String ()
validateConsumeToken name ct = validateGuardedStreamExists name
  ((^. consumeToken) >>> fmap fst >>> assertEq "invalid consume token" (Just ct))

updatePushToken :: Text -> PushToken -> UTCTime -> TokenSet -> Either String TokenSet
updatePushToken name newToken tokenCreationTime =
  updateGuardedStreamExists name (pushToken ?~ (newToken, tokenCreationTime) >>> pure)

updateReadToken
  :: Text
  -> Maybe ReadToken
  -> Maybe (ReadToken, StreamReadState, UTCTime)
  -> TokenSet
  -> Either String TokenSet
updateReadToken name oldToken newTokenInfo =
  updateGuardedStreamExists name (pure . addNewToken . removeOldToken)
  where
    removeOldToken = oldToken & maybe id (\ot -> readTokens . at ot .~ Nothing)
    addNewToken    = newTokenInfo & maybe id (\(rt, rs, t) -> readTokens . at rt ?~ (rs, t))

updateConsumeToken :: Text -> Maybe (ConsumeToken, UTCTime) -> TokenSet -> Either String TokenSet
updateConsumeToken name newTokenInfo =
  updateGuardedStreamExists name (consumeToken .~ newTokenInfo >>> pure)

purgePushTokenOlderThan :: Text -> UTCTime -> TokenSet -> Either String TokenSet
purgePushTokenOlderThan name t =
  updateGuardedStreamExists name (pushToken %~ maybe Nothing (_2 $ ensureNoOlderThan t) >>> pure)

purgeReadTokensOlderThan :: Text -> UTCTime -> TokenSet -> Either String TokenSet
purgeReadTokensOlderThan name t =
  updateGuardedStreamExists name (readTokens %~ Map.filter ((t <=) . snd) >>> pure)

purgeConsumeTokenOlderThan :: Text -> UTCTime -> TokenSet -> Either String TokenSet
purgeConsumeTokenOlderThan name t =
  updateGuardedStreamExists name (consumeToken %~ maybe Nothing (_2 $ ensureNoOlderThan t) >>> pure)

hasReadTokens :: Text -> TokenSet -> Either String Bool
hasReadTokens name =
  queryGuardedStreamExists name ((^. readTokens) >>> Map.null)

hasConsumeToken :: Text -> TokenSet -> Either String Bool
hasConsumeToken name =
  queryGuardedStreamExists name ((^. consumeToken) >>> isJust)

addStreamIfNotExists :: Text -> TokenSet -> TokenSet
addStreamIfNotExists name = at name %~ Just . fromMaybe newStreamTokenSet
