{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE TupleSections              #-}

module Data.Immutaly.Internal.StreamChunk.Builder (
  Builder,
  chunk,
  delim,
  toStreamChunks,
) where

import           Data.Bifunctor
import           Data.Extensible                          hiding (item)
import           Data.Immutaly.Internal.StreamChunk.Types
import           Data.Immutaly.Internal.Util.Composite
import           Debug.Trace

data BuilderItem m = Data m | Delim deriving Show

newtype Builder m =
  Builder
    { unBuilder :: Composite (BuilderItem m)
    } deriving (Show, Semigroup, Monoid)

chunk :: m -> Builder m
chunk = Builder . item . Data

delim :: Builder m
delim = Builder $ item Delim

toStreamChunks :: Monoid m => Builder m -> ChunkStart -> (StreamChunks m, ChunkStart)
toStreamChunks b chunkStart =
  (, chunkStart') $
     #start   @= chunkStart
  <: #end     @= chunkEnd
  <: #chunks  @= chunks
  <: nil
  where (chunks, chunkEnd) = buidlerItemListToStreamChunkList (toList $ unBuilder b)
        chunkStart' = nextChunkStart chunkEnd

buidlerItemListToStreamChunkList :: Monoid m => [BuilderItem m]  -> ([m], ChunkEnd)
buidlerItemListToStreamChunkList []             = ([], ToBeContinued)
buidlerItemListToStreamChunkList [Delim]        = ([mempty], End)
buidlerItemListToStreamChunkList [Data m]       = ([m], ToBeContinued)
buidlerItemListToStreamChunkList (Data m : bis) = (m <> m' : ms', end')
  where (m':ms', end') = buidlerItemListToStreamChunkList bis
buidlerItemListToStreamChunkList (Delim : bis)  = (mempty : ms', end')
  where (ms', end') = buidlerItemListToStreamChunkList bis
