{-# LANGUAGE RankNTypes #-}
module Data.Immutaly.Internal.InMemoryImmutalyProvider.Util where
import Data.Text (Text)
import Data.Time
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

ensureNoOlderThan :: UTCTime -> UTCTime -> Maybe UTCTime
ensureNoOlderThan t0 t1
  | t0 < t1   = Nothing
  | otherwise = Just t1

assertEq :: Eq a => String -> a -> a -> Either String ()
assertEq errMsg a1 a2
  | a1 == a2  = pure ()
  | otherwise = Left errMsg

_SeqLast :: Lens' (Seq a) (Maybe a)
_SeqLast f s = fmap (setSeqLast s) . f $ getSeqLast s
  where
    getSeqLast = Seq.lookup i
    setSeqLast s a = maybe id (Seq.update i) a s
    i = Seq.length s - 1