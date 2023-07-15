module Shuffle
  ( fisherYates,
    fisherYatesVector,
    shuffle,
    randomShuffle,
    randomShuffleVector,
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
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Numeric.Natural (Natural)
import System.Random (getStdRandom, mkStdGen, randomIO, uniformR)
import System.Random.Stateful (RandomGenM, newSTGenM, randomRM)

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
 - The Fisher-Yates algorithm applied to a mutable vector.
 -}
fisherYatesVector ::
  (GM.PrimMonad m, GM.MVector v a, RandomGenM g r m) =>
  g -> v (GM.PrimState m) a -> m (v (GM.PrimState m) a)
fisherYatesVector rng vm = do
  let l = pred $ GM.length vm
  mapM_ (\i -> randomRM (i, l) rng >>= GM.swap vm i) $ enumFromTo 0 (pred l)
  return vm

{-
 - Randomly shuffle a list as a vector.
 -}
randomShuffleVector :: (MonadIO m, G.Vector v a) => [a] -> m (v a)
randomShuffleVector list = do
  seed <- randomIO
  return $ G.create $ do
    rng <- newSTGenM (mkStdGen seed)
    vm <- (G.unsafeThaw . G.fromList) list
    fisherYatesVector rng vm
