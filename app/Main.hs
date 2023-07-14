module Main (main) where

import Shuffle (randomShuffleVector)

main :: IO ()
main = words <$> getContents >>= randomShuffleVector >>= mapM_ putStrLn
