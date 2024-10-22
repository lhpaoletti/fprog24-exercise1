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
et (n, k) =
    let valid     = n >= 0 && k >= 0
        left_side = sum [f x k | x <- [k + i | i <- [0..(n - k)]]]  -- sum of (f k+i k), where i is in [0, n - k]
    in if valid
       then left_side == f (n + 1) (k + 1)
       else error "n and k must be Nat0"
