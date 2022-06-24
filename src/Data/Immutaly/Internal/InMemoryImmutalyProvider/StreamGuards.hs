{-# Language OverloadedStrings #-}
module Data.Immutaly.Internal.InMemoryImmutalyProvider.StreamGuards where
import Data.Text (Text)
import Fmt
import Control.Lens
import Control.Arrow ((>>>))
import Data.Map (Map)

validateGuardedStreamExists
  :: Text
  -> (a -> Either String b)
  -> Map Text a
  -> Either String b
validateGuardedStreamExists name f = (^. at name)
  >>> maybe (Left $ "stream "+|name|+" does not exist") f

queryGuardedStreamExists
  :: Text
  -> (a -> b)
  -> Map Text a
  -> Either String b
queryGuardedStreamExists name f = (^. at name)
  >>> maybe (Left $ "stream "+|name|+" does not exist") (pure . f)

updateGuardedStreamExists
  :: Text
  -> (a -> Maybe a)
  -> Map Text a
  -> Either String (Map Text a)
updateGuardedStreamExists name f = at name $
  maybe (Left $ "stream "+|name|+" does not exist")
        (pure . f)

tryUpdateGuardedStreamExists
  :: Text
  -> (a -> Either String (Maybe a))
  -> Map Text a
  -> Either String (Map Text a)
tryUpdateGuardedStreamExists name f = at name $
  maybe (Left $ "stream "+|name|+" does not exist")
        f