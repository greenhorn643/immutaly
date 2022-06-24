module Main where

import Lib
import Data.Time

exampleTime :: UTCTime
exampleTime = read "2011-11-19 18:28:52.607875 UTC"

main :: IO ()
main = print exampleTime
