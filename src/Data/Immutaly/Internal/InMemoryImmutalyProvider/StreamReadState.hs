{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamReadState where
import           Data.Extensible
import           Data.Int

type StreamReadState = Record
  [ "fragmentIndex" :> Int64
  , "byteOffset"    :> Int64
  ]

mkField "fragmentIndex byteOffset"

newStreamReadState :: StreamReadState
newStreamReadState =
     #fragmentIndex @= 0
  <: #byteOffset    @= 0
  <: nil
