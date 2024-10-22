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
    let valid     = n >= 0 && k >= 0
        nominator = product [n - j | j <- [0..(k - 1)]]  -- product of (n - j), where j is in [0, k - 1]
    in if valid
       then div nominator . product $ [1..k]             -- nominator / k!
       else error "n and k must be Nat0"

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
