module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Numeric.Natural (Natural)
import Shuffle
  ( factoradicBE,
    factoradicLE,
    factorial,
    invertFactoradicBE,
    invertFactoradicLE,
    shuffleVector,
  )
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
  quickCheck prop_factoradicBE_invertibe_modulo_factorial
  quickCheck prop_factoradicLE_invertibe_modulo_factorial
  quickCheck prop_shuffleVector_reproducible

{-
 - Test the invertibility property of the factoradic functions modulo
 - the factorial of a given factoradic length.
 -}
prop_factoradicBE_invertibe_modulo_factorial :: Word -> Word -> Bool
prop_factoradicBE_invertibe_modulo_factorial l n =
  ((invertFactoradicBE .) . factoradicBE) l' n' == mod n' (factorial l')
  where
    l' = fromIntegral l :: Natural
    n' = fromIntegral n :: Natural

prop_factoradicLE_invertibe_modulo_factorial :: Word -> Word -> Bool
prop_factoradicLE_invertibe_modulo_factorial l n =
  ((invertFactoradicLE .) . factoradicLE) l' n' == mod n' (factorial l')
  where
    l' = fromIntegral l :: Natural
    n' = fromIntegral n :: Natural

prop_shuffleVector_reproducible :: Int -> [String] -> Bool
prop_shuffleVector_reproducible seed list =
  G.all (\(s, i) -> s == v G.! i) $ G.zip shuffled indices
  where
    v = V.fromList list
    shuffled = shuffleVector seed list :: V.Vector String
    indices = shuffleVector seed (enumFromTo 0 $ length list - 1) :: V.Vector Int
