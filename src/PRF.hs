{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module PRF (module PRF) where
data Nat = Zero | Succ Nat deriving Show

-- recNat is a higher-order recursor that receives three parameters:
-- the value of f(0), a function h, and the element used for the recursion.
-- The recursion element refers to the number of times it will iterate until
-- the recursion is completed. When the recursion element gets to 0, the
-- recursion takes place and the function h is performed to each step of the
-- recursion until it returns a value.
recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

-- addR is the primitive recursive function for addition. This function recieve
-- two parameters: a natural number n and a natural number m. In order to sum
-- up this numbers we use m as the recursion element and the function h as the
-- succesor function that sums up 1 to n (the f(0) value) of each recursive
-- iteration.
addR :: Nat -> Nat -> Nat
addR m n = recNat n (\_ y -> Succ y) m

-- identityR is the identity primitive recursive function. Given certain
-- natural number n this function should return this same number. In order to
-- do this we use 0 as the f(0) value, n as the recursion element and the
-- successor function as h. This sums n times 1 to f(0) and finally returns
-- the same natural number.
identityR :: Nat ->Nat
identityR n = recNat Zero (\_ y -> Succ y) n

-- constant3R function is the constant primitive recursive function for 3.
-- This means that given any number to the function, it should return 3. For
-- doing this we use 0 as the f(0) value, 3 as the recursion element and the
-- successor function as h. Since 3 is the recursion element, there would be
-- three iterations which will be adding 1 by 1 to the f(0) value.
constant3R :: Nat -> Nat
constant3R _ = recNat Zero (\_ y -> Succ y) (Succ (Succ (Succ Zero)))

-- predR is the predecessor primitive recursive function. Given certain natural
-- number n, the function should return n-1. In this case we take advangtage of
-- the structure of the function recNat, since in the first recursive call the
-- n evaluated in the function h is the predecessor. So we use the identity
-- function as h so it returns the n value of the last iteration.
predR :: Nat -> Nat
predR n= recNat Zero (\x _ -> x) n

-- multiplyR is the primitive recursive function for multiplication. Given two
-- natural numbers, n and m, this function should return the multiplication of
-- those two numbers. In order to accomplish multiplication, we take 0 as the
-- f(0), the addition function between f(0) and n as the h function, and m as
-- the recursion element. This will add un m times n to 0, finally returning
-- the product of those two numbers.
multiplyR:: Nat -> Nat -> Nat
multiplyR n m = recNat Zero (\_ y -> addR n y) m

-- signumR is the signum primitive recursive function. Given a natural number n
-- this function returns 0 if n its equal to 0 and 1 otherwise. In this case we
-- simply take n as the recursion element, f(0) as 0 and use the constant1
-- function as h. If n is greater than 0 it means that there would be recursive
-- iterations and the function returns 1, but if its equal to 0 it returns 0.
signumR :: Nat -> Nat
signumR n = recNat Zero (\_ _ -> Succ Zero) n
