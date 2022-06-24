{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Immutaly.Internal.InMemoryImmutalyProvider.SchemaRegister where
import Data.Extensible
import Data.Text (Text)
import Data.Word
import Data.Map (Map)
import Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamGuards
import Data.Immutaly.Internal.InMemoryImmutalyProvider.Util
import Control.Arrow ((>>>))
import Control.Lens hiding ((:>))
import Data.Maybe
import Data.Extensible.StreamFmt.Schema
import Data.Hashable
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B

type SchemaEntry = Record
  [ "datumSchema"      :> Text
  , "datumSchemaHash" :> Word64
  ]

mkField "datumSchema datumSchemaHash"

schemaHash :: forall a. Schema a => Word64
schemaHash = fromIntegral . hash . B.toLazyText $ schema @a

schemaText :: forall a. Schema a => Text
schemaText = LT.toStrict . B.toLazyText $ schema @a

schemaEntry :: forall a. Schema a => SchemaEntry
schemaEntry = #datumSchema @= schemaText @a <: #datumSchemaHash @= schemaHash @a <: nil

type SchemaRegister = Map Text SchemaEntry

validateSchema :: Text -> Word64 -> SchemaRegister -> Either String ()
validateSchema name h = validateGuardedStreamExists name $
  (^. datumSchemaHash) >>> assertEq "invalid schema hash" h

addStreamSchemaIfNotExists :: Text -> SchemaEntry -> SchemaRegister -> SchemaRegister
addStreamSchemaIfNotExists name se = at name %~ Just . fromMaybe se