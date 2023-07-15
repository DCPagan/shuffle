module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector, toList)
import Shuffle (randomShuffleVector)

main :: IO ()
main =
  T.lines <$> TIO.getContents
    >>= (randomShuffleVector :: [T.Text] -> IO (Vector T.Text))
    >>= TIO.putStr . T.unlines . toList
