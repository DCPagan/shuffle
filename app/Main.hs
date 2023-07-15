module Main (main) where

import Data.Vector (Vector)
import Shuffle (randomShuffleVector)

main :: IO ()
main =
  words <$> getContents
    >>= (randomShuffleVector :: [String] -> IO (Vector String))
    >>= mapM_ putStrLn
