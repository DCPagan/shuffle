module Main (main) where

import Numeric.Natural (Natural)
import Shuffle
  ( factoradicBE,
    factoradicLE,
    factorial,
    invertFactoradicBE,
    invertFactoradicLE
  )
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
  quickCheck prop_factoradicBE_invertibility_modulo_factorial
  quickCheck prop_factoradicLE_invertibility_modulo_factorial

{-
 - Test the invertibility property of the factoradic functions modulo
 - the factorial of a given factoradic length.
 -}
prop_factoradicBE_invertibility_modulo_factorial :: Word -> Word -> Bool
prop_factoradicBE_invertibility_modulo_factorial l n =
  ((invertFactoradicBE .) . factoradicBE) l' n' == mod n' (factorial l')
    where
      l' = fromIntegral l :: Natural
      n' = fromIntegral n :: Natural

prop_factoradicLE_invertibility_modulo_factorial :: Word -> Word -> Bool
prop_factoradicLE_invertibility_modulo_factorial l n =
  ((invertFactoradicLE .) . factoradicLE) l' n' == mod n' (factorial l')
    where
      l' = fromIntegral l :: Natural
      n' = fromIntegral n :: Natural
