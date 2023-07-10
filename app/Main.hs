module Main (main) where

import Control.Monad ((>=>))
import Shuffle (randomShuffle)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM_ (randomShuffle >=> putStrLn)
