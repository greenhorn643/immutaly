{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Immutaly.Internal.InMemoryImmutalyProvider.Token where

import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Serialize
import           GHC.Generics
import           System.Random

newtype TokenFragment =
  TokenFragment
    { unTokenFragment :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Serialize)

newtype PushToken =
  PushToken
    { unPushToken :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Serialize)

newtype ReadToken =
  ReadToken
    { unReadToken :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Serialize)

newtype ConsumeToken =
  ConsumeToken
    { unConsumeToken :: ByteString
    }
    deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Serialize)

nextTokenFragment :: RandomGen g => g -> (TokenFragment, g)
nextTokenFragment = first (TokenFragment . B.pack) . generate 32 random
  where generate :: Int -> (g -> (a, g)) -> g -> ([a], g)
        generate 0 _ g = ([], g)
        generate n f g = (a:as, g'')
          where (a, g')   = f g
                (as, g'') = generate (n - 1) f g'
