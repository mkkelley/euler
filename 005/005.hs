module Main where
needed_factors = [1..20]

sum_mod n factors = foldl plus_n_mod 0 factors
    where
        plus_n_mod a b = a + (mod n b)

small_n_w_fac_impl n factors
    | sum_mod n factors == 0 = n
    | otherwise = small_n_w_fac_impl (n+1) factors

smallest_n_with_factors :: Integral a => [a] -> a
smallest_n_with_factors factors = small_n_w_fac_impl 1 factors

main = do
    putStrLn $ show $ smallest_n_with_factors [1..20]
