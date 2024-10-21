{- A1 Solution
 -
 - Author: Luis Henrique Paoletti, 11929823
 - Date: 21.10.2024
 -}
module A1 where
import Types


type N_value = Nat0
type K_value = Nat0


{- Mathematical function as described in A1. -}
f :: N_value -> K_value -> Nat0
f n k =
    let result = (nominator_sum k n) `div` (denominator_sum k)
    in if result < 0
       then error "Illegal arguments"
       else result

{- Nominator sum function for f.
 - The sum is from i=0 to (k - 1). The expression is (n - i).
 -}
nominator_sum :: K_value -> N_value -> Int
nominator_sum k n = nominator_sum' (k - 1) n 0

nominator_sum' :: Nat0 -> N_value -> Int -> Int
nominator_sum' counter n acc
    | counter < 0 = acc
    | otherwise   = nominator_sum' (counter - 1) n (acc + n - counter)

{- Denominator sum function for f.
 - The sum is from i=1 to k. The expression is (i);
 -}
denominator_sum :: K_value -> Int
denominator_sum k = denominator_sum' k 0

denominator_sum' :: Nat0 -> Int -> Int
denominator_sum' counter acc
    | counter <= 0 = acc
    | otherwise    = denominator_sum' (counter - 1) (acc + counter)

{- Mathematical test as described in A1. -}
et :: (N_value, K_value) -> Bool
et (n, k) = et_sum n k == f (n + 1) (k + 1)

{- Sum function for the left side of et.
 - The sum is from i=0 to (n - k). The expression is (f (k + i) k).
 -}
et_sum :: N_value -> K_value -> Nat0
et_sum n k =
    let result = et_sum' (n - k) k 0
    in if result < 0
       then error "Illegal arguments"
       else result

et_sum' :: Nat0 -> K_value -> Int -> Int
et_sum' counter k acc
    | counter < 0 = acc
    | otherwise   = et_sum' (counter - 1) k (acc + f (k + counter) k)
