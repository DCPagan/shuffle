module Shuffle
  ( fisherYates,
    fisherYatesVector,
    shuffle,
    shuffleVector,
    randomShuffle,
    randomShuffleVector,
    factorial,
    factorialRange,
    factoradicBE,
    factoradicLE,
    ulength,
    newSTFromList,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.ST
import Data.Foldable (foldl')
import Data.Ix (range)
import Data.List (unfoldr)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Numeric.Natural (Natural)
import System.Random (getStdRandom, uniformR)

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
 - Swap the first element of a list with that of the given index.
 -}
swap :: Natural -> [a] -> [a]
swap _ [] = []
swap 0 l = l
swap i l = case splitAt (fromIntegral i) l of
  ([], _) -> l
  (_, []) -> l
  (x : first, y : second) -> y : first ++ x : second

{-
 - A purely functional, lazy implementation of the Fisher-Yates algorithm,
 - which derives its swaps from a given (big-endian) factoradic rather than
 - calculating each swap randomly.
 -}
fisherYates :: [a] -> [Natural] -> [a]
fisherYates ls [] = ls
fisherYates ls (s : ss) = case swap s ls of
  [] -> ls
  y : ys -> y : fisherYates ys ss

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
 - Randomly shuffle a list.
 -}
randomShuffle :: MonadIO m => [a] -> m [a]
randomShuffle = fmap <$> shuffle <*> getStdRandom . curry uniformR 0 . pred . factorial . ulength

{-
 - Initialize a state thread with a vector of the given data.
 -}
newSTFromList :: VM.PrimMonad m => [a] -> m (VM.MVector (VM.PrimState m) a)
newSTFromList list = do
  vm <- VM.new (length list)
  mapM_ (\(i, x) -> VM.write vm i x) (zip (enumFrom 0) list)
  return vm

{-
 - The Fisher-Yates algorithm applied to a mutable vector.
 -}
fisherYatesVector :: VM.PrimMonad m => VM.MVector (VM.PrimState m) a -> [Natural] -> m ()
fisherYatesVector vm swaps =
  mapM_ (\(i, j) -> VM.swap vm i (i + j)) (zip (enumFrom 0) (fmap fromIntegral swaps))

{-
 - Shuffle over a vector given a permutation serial.
 -}
shuffleVector :: VM.PrimMonad m =>
  VM.MVector (VM.PrimState m) a -> Natural -> m ()
shuffleVector vm n = do
  let swaps = factoradicBE (fromIntegral $ VM.length vm) n
  fisherYatesVector vm swaps

{-
 - Randomly shuffle a list as a vector.
 -}
randomShuffleVector :: MonadIO m => [a] -> m (V.Vector a)
randomShuffleVector list = do
  n <- getStdRandom $ uniformR (0, factorial (ulength list) - 1)
  return $ runST $ do
    vm <- newSTFromList list
    shuffleVector vm n
    V.freeze vm
