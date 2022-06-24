{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Immutaly.Internal.Types where
import Data.Serialize
import Data.Extensible.StreamFmt.Schema
import Data.Text (Text)
import Data.Typeable
import Data.Kind
import Control.Monad.Trans.State.Strict
import Data.Extensible (KeyTargetAre, KnownSymbol, Forall)
import GHC.Generics

data TokenType = Push | Read | Consume

data ChunkStart = Beginning | Continuation
  deriving (Generic)

data ChunkEnd = End | ToBeContinued
  deriving (Generic)

instance Serialize ChunkStart
instance Serialize ChunkEnd

type StreamChunk a = (ChunkStart, [a], ChunkEnd)

class Monad m => ImmutalyProvider m p where
  data Token p :: TokenType -> Type
  verifySchema ::
    forall xs. Forall (KeyTargetAre KnownSymbol Schema) xs
    => Proxy xs -> StateT p m ()
  reifySchema :: 
    forall xs. Forall (KeyTargetAre KnownSymbol Schema) xs
    => Proxy xs -> StateT p m ()
  pushToStream :: (Schema a, Serialize a, Foldable t) => t a -> Maybe (Token p Push) -> StateT p m (Token p Push)
  readFromStream :: (Schema a, Serialize a) => Int -> Maybe (Token p 'Read) -> StateT p m ([StreamChunk a], Maybe (Token p 'Read))
  consumeFromStream :: (Schema a, Serialize a) => Int -> Maybe (Token p 'Consume) -> StateT p m ([StreamChunk a], Maybe (Token p 'Consume))