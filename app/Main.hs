module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Shuffle (shuffleVector)
import System.Random (randomIO)

main :: IO ()
main =
  shuffleVector <$> randomIO <*> (T.lines <$> TIO.getContents)
    >>= TIO.putStr . T.unlines . V.toList
