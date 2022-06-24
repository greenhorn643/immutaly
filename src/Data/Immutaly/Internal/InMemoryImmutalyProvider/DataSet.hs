{-# LANGUAGE OverloadedStrings #-}
module Data.Immutaly.Internal.InMemoryImmutalyProvider.DataSet where

import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq ((:<|)))
import Data.Map (Map)
import Data.Text (Text)
import Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamGuards
import Data.Immutaly.Internal.InMemoryImmutalyProvider.Util
import Control.Lens
import Data.Maybe
import Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamReadState
import Data.Immutaly.Internal.StreamChunk.Builder (Builder)
import Data.Immutaly.Internal.StreamChunk
import qualified Data.Immutaly.Internal.StreamChunk.Builder as B
import Data.Int
import qualified Data.Sequence as Seq
import Data.Bifunctor

type StreamDataSet = Seq ByteString
type DataSet = Map Text StreamDataSet

startNewStreamFragment :: Text -> DataSet -> Either String DataSet
startNewStreamFragment name = updateGuardedStreamExists name f
  where f :: StreamDataSet -> Maybe StreamDataSet
        f = Just . (|> "")

updatedGuardedFragmentExists :: ByteString -> StreamDataSet -> Either String StreamDataSet
updatedGuardedFragmentExists newData = _SeqLast $
  maybe (Left "stream fragment does not exist")
        (Right . Just . (<> newData))

writeToStream :: Text -> ByteString -> DataSet -> Either String DataSet
writeToStream name newData = tryUpdateGuardedStreamExists name $
  fmap Just <$> updatedGuardedFragmentExists newData

rewindReadStateOnFragments :: Int64 -> StreamReadState -> StreamDataSet -> Either String StreamReadState
rewindReadStateOnFragments nBytesToRewind rs fragments = Seq.lookup (fromIntegral fi) fragments &
  maybe (Left "invalid stream read state") go
  where go _ = if bo >= nBytesToRewind
                then Right (rs & byteOffset -~ nBytesToRewind)
                else rewindReadStateOnFragments nBytesToRewind' rs' fragments
                where nBytesToRewind' = nBytesToRewind - bo
                      rs'             = rs & fragmentIndex -~ 1
        fi = rs ^. fragmentIndex
        bo = rs ^. byteOffset

rewindReadStateOnDataSet :: Int64 -> StreamReadState -> Text -> DataSet -> Either String StreamReadState
rewindReadStateOnDataSet nBytesToRewind readState name = validateGuardedStreamExists name $
  rewindReadStateOnFragments nBytesToRewind readState

readFromFragment :: Int64 -> Int64 -> ByteString -> (ByteString, Int64, Int64)
readFromFragment nBytesToRead byteOffset rawData = (readData, nBytesToRead', byteOffset')
  where readData      = LB.take nBytesToRead $ LB.drop byteOffset rawData
        nBytesRead    = LB.length readData
        nBytesToRead' = nBytesToRead - nBytesRead
        byteOffset'   = byteOffset + nBytesRead

readFromFragments :: Int64 -> StreamReadState -> StreamDataSet -> (Builder ByteString, Maybe StreamReadState)
readFromFragments nBytesToRead rs fragments =
  maybe (mempty, Nothing) (go nBytesToRead) (Seq.lookup (fromIntegral fi) fragments)
  where go 0 _    = (mempty, Just rs)
        go _ frag = (chunks', rs'')
          where (chunk, nBytesToRead', bo') = readFromFragment nBytesToRead bo frag
                (chunks, rs'')              = readFromFragments nBytesToRead' rs' fragments
                rs' = if bo' == LB.length frag
                      then rs & fragmentIndex +~ 1 & byteOffset .~ 0
                      else rs & byteOffset .~ bo'
                chunks' = if nBytesToRead' /= 0 || bo' == LB.length frag
                          then B.chunk chunk <> B.delim <> chunks
                          else B.chunk chunk
        fi = rs ^. fragmentIndex
        bo = rs ^. byteOffset

readFromDataSet :: Int64 -> StreamReadState -> Text -> DataSet -> Either String (StreamChunks ByteString, Maybe StreamReadState)
readFromDataSet nBytesToRead rs name = queryGuardedStreamExists name go
  where go         = first (fst . flip B.toStreamChunks chunkStart) . readFromFragments nBytesToRead rs
        chunkStart = if rs ^. byteOffset == 0 then Beginning else Continuation

addDataStreamIfNotExists :: Text -> DataSet -> DataSet
addDataStreamIfNotExists name = at name %~ Just . fromMaybe mempty