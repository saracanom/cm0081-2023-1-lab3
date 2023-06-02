--This line avoids the warning caused by the orphan Arbitrary instance for
--the data typeNatural.
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where
--Here we import all the functions and constructors that will be used in the
--tests
import PRF
import Numeric.Natural ( Natural )
import Test.QuickCheck (Arbitrary(..), arbitrarySizedNatural, shrinkIntegral, quickCheck )

--For the Arbitrary instance for the Natural data type.
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral

natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (Succ n) = 1 + natToNatural n

naturalToNat :: Natural -> Nat
naturalToNat 0 = Zero
naturalToNat n = Succ (naturalToNat (n - 1))

prop_addR :: Natural -> Natural -> Bool
prop_addR m n = natToNatural (addR m' n') == m + n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n
-- This function checks that the function addR gives the same result as using
-- the addition operator. This function receives a two natural numbers as
-- input and inside the function, the naturalToNat function is called in order
-- to convert these naturals into the data type Nat.The functions return True
-- if both return the same results.

prop_identityR :: Natural -> Bool
prop_identityR n = natToNatural (identityR n') == n
    where
    n':: Nat
    n' = naturalToNat n
-- This function checks that the function identityR gives the same result as
-- the input given. This function receives a natural number as input and
-- returns a boolean value. The functions return True if both return the same
-- results.

prop_constant3R ::Natural -> Bool
prop_constant3R n = natToNatural (constant3R n') == 3
    where
    n':: Nat
    n' = naturalToNat n
-- This function checks that the funtion constant3R gives 3 as a result. This
-- function receives a natural number as input and returns a boolean. The
-- function returns True if constant3R returns 3.

prop_predR :: Natural -> Bool
prop_predR 0 = natToNatural (predR Zero) == 0
prop_predR n  = natToNatural (predR n') == n - 1
    where
    n':: Nat
    n' = naturalToNat n
-- This function checks that the funtion predR gives n's predecessor as a
-- result. This function receives a natural number as input and returns a
-- boolean. The function returns True if predR returns n - 1. In order to
-- avoid negative numbers as a result, we had to define a base case for when
-- n is 0 so that our predR funtion can be called with Zero as input. In the
-- other cases, prop_predR is called with n.

prop_signumR :: Natural -> Bool
prop_signumR n
    | n == 0 = natToNatural (signumR n') == 0
    | otherwise = natToNatural (signumR n') == 1
    where
    n':: Nat
    n' = naturalToNat n
-- This function checks that the funtion signumR returns 0 as a result if n is
-- equal to 0, and 1 otherwise. This function receives a natural number as
-- input and returns a boolean. The function returns True if signumR returns
-- 0 if n is 0 and 1 otherwise. In order to evaluate the signumR function with
-- 0 and other values, we used the "|" guard.A guard is a boolean expression.
-- If it evaluates to True, then the corresponding function body is used. If
-- it evaluates to False, checking drops through to the next guard and so on.
-- So when n == 0, the guard calls the signumR function with n' as input and
-- returns True if it returns 0. Otherwise, the guard returns True if signumR
-- returns 1. Taken fron:
-- http://igm.univ-mlv.fr/~vialette/teaching/2021-2022/haskell/lectures/
-- lecture-04.pdf

prop_multiplyR :: Natural -> Natural -> Bool
prop_multiplyR m n = natToNatural (multiplyR m' n') == m * n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n
-- This function checks that the function multiplyR gives the same result as
-- using the multiplication operator. This function receives a two natural
-- numbers as input and returns a bollean.The functions return True if both
-- return the same results.

main :: IO()
main = do
    quickCheck prop_addR
    quickCheck prop_identityR
    quickCheck prop_constant3R
    quickCheck prop_predR
    quickCheck prop_signumR
    quickCheck prop_multiplyR
-- Main function declaration, the IO () type constructor allows the
-- representation of actions as Haskell values.
-- The main function is where the tests will be carried out.This function calls
-- quickcheck for each property. Whe a test returns True, this will be the
-- output in the terminal.
-- +++ OK, passed 100 tests.