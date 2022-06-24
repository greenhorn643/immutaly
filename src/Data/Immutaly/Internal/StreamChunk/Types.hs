{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module Data.Immutaly.Internal.StreamChunk.Types where
import           Data.Extensible
import           Data.Serialize
import           GHC.Generics

data ChunkStart = Beginning | Continuation
  deriving (Generic, Show, Eq)

data ChunkEnd = End | ToBeContinued
  deriving (Generic, Show, Eq)

instance Serialize ChunkStart
instance Serialize ChunkEnd

nextChunkStart :: ChunkEnd -> ChunkStart
nextChunkStart End           = Beginning
nextChunkStart ToBeContinued = Continuation

type StreamChunks m = Record
  [ "start"   :> ChunkStart
  , "end"     :> ChunkEnd
  , "chunks"  :> [m]
  ]

mkField "start end chunks"
