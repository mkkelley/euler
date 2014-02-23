module Factor where

ifind_lowest_factor _ last_factor 0 = last_factor
ifind_lowest_factor x factor remainder
    | (fromIntegral factor) < (sqrt $ fromIntegral x) = ifind_lowest_factor x (factor + 1) (x `mod` (factor + 1))
    | otherwise = x

find_lowest_factor x = ifind_lowest_factor x 1 1

can_be_factored x = (find_lowest_factor x) /= x

is_factor_of factor number = (number `mod` factor) == 0

factor :: Integral a => a -> [a]
factor x
    | can_be_factored x = low_factor : factor (quot x low_factor)
    | otherwise = [x]
    where low_factor = (find_lowest_factor x)
