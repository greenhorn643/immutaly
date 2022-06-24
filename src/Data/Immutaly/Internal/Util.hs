{-# LANGUAGE RankNTypes #-}
module Data.Immutaly.Internal.Util where
import Control.Monad.Trans.State.Strict
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.Trans.Class

lensPair :: Lens' s a -> Lens' s b -> Lens' s (a, b)
lensPair la lb f s = fmap setAB . f $ getAB
  where setAB (a, b) = s & la .~ a & lb .~ b
        getAB = (s ^. la, s ^. lb)

_SeqLastUsafe :: Lens' (Seq a) a
_SeqLastUsafe f s = fmap (setSeqLast s) . f $ getSeqLast s
  where
    getSeqLast s = Seq.index s i
    setSeqLast s a = Seq.update i a s
    i = Seq.length s - 1

_MapIxUnsafe :: Ord k => k -> Lens' (Map k v) v
_MapIxUnsafe k f s = fmap (setMap s) . f $ getMap
  where
    getMap = s ! k
    setMap s v = Map.insert k v s

foldlWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldlWhile _ _ b []     = b
foldlWhile p f b (a:as)
  | p b       = foldlWhile p f (f b a) as
  | otherwise = b

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM f = get >>= lift . f >>= put