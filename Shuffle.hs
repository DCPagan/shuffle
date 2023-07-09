module Shuffle
  ( fisherYates,
    shuffle,
    shuffleTuple,
    printShuffles,
    factorial,
    factorialRange,
    factoradicBE,
    factoradicLE,
    ulength,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (foldl')
import Data.Ix (range)
import Data.List (unfoldr)
import Data.Tuple (curry)
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import System.Random (getStdRandom, uniformR)
import Control.Monad (void)

ulength :: Foldable t => t a -> Natural
ulength = foldl' (\c _ -> c + 1) 0

-- Calculate the factorial of an integer.
factorial :: Natural -> Natural
factorial = product . curry range 1

{-
 - Calculate [0, n!-1), useful for serializing all permutations of a given
 - length.
 -}
factorialRange :: Natural -> [Natural]
factorialRange = curry range 0 . pred . factorial

{-
 - Calculate the (big-endian) factoradic representation for a given length
 - of a natural number.
 -}
factoradicBE :: Natural -> Natural -> [Natural]
factoradicBE = (unfoldr f .) . params
  where
    params = (.) <$> ((,,) <*> factorial . pred) <*> flip mod . factorial
    f (i, j, n)
      | i == 0 = Nothing
      | otherwise = Just (div n j, (k, div j k, mod n j))
      where
        k = pred i

{-
 - Calculate the (little-endian) factoradic representation for a given length
 - of a natural number.
 -}
factoradicLE :: Natural -> Natural -> [Natural]
factoradicLE = (. (1,)) . unfoldr . f
  where
    f l (i, n)
      | i > l = Nothing
      | otherwise = Just (mod n i, (succ i, div n i))

{-
 - A purely functional, lazy implementation of the Fisher-Yates algorithm,
 - which derives its swaps from a given (big-endian) factoradic rather than
 - calculating each swap randomly.
 -}
fisherYates :: [a] -> [Natural] -> [a]
fisherYates ls [] = ls
fisherYates ls (s : ss) = y : fisherYates ys ss
  where
    y : ys = swap s ls
    swap 0 l = l
    swap i l = y : first ++ x : second
      where
        (x : first, y : second) = splitAt (fromIntegral i) l

{-
 - A purely functional, lazy implementation of a shuffle algorithm, which
 - takes a natural number representing the permuation.
 -
 - This natural number can be derived from a random number generator with a
 - range of [0, n!), where n is the length of the list.
 -}
shuffle :: [a] -> Natural -> [a]
shuffle = (.) <$> fisherYates <*> factoradicBE . ulength

{-
 - List all permutations for a given list.
 -}
permutations :: [a] -> [[a]]
permutations = map <$> shuffle <*> factorialRange . ulength

{-
 - Get the shuffle for a given array along with its permutation's serial and
 - (big-endian) factoradic.
 -}
shuffleTuple :: [a] -> Natural -> (Natural, [Natural], [a])
shuffleTuple =
  curry $
    (,,)
      <$> uncurry seq
      <*> uncurry (factoradicBE . ulength)
      <*> uncurry shuffle

{-
 - Print all shuffles of a given array in tuples with its permutation serial
 - and its (big-endian) factoradic.
 -}
printShuffles :: Show a => [a] -> IO ()
printShuffles = mapM_ <$> (print .) . shuffleTuple <*> factorialRange . ulength

{-
 - Randomly shuffle a list.
 -}
randomShuffle :: MonadIO m => [a] -> m [a]
randomShuffle = fmap <$> shuffle <*> getStdRandom . curry uniformR 0 . pred . factorial . ulength

main :: IO ()
main = getArgs >>= mapM randomShuffle >>= mapM_ putStrLn
