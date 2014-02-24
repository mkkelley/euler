module Factor where

ifind_lowest_factor :: Integral a => a -> a -> a -> a
ifind_lowest_factor _ last_factor 0 = last_factor
ifind_lowest_factor x factor remainder
    | (fromIntegral factor) < (sqrt $ fromIntegral x) = ifind_lowest_factor x (factor + 1) (x `mod` (factor + 1))
    | otherwise = x

-- starts with last_factor of 1 to skip it (factor of all but 0)
-- starts with remainder of 1 to stop immediate exit
find_lowest_factor x = ifind_lowest_factor x 1 1

can_be_factored :: Integral a => a -> Bool
can_be_factored x = (find_lowest_factor x) /= x

is_factor_of :: Integral a => a -> a -> Bool
is_factor_of factor number = (number `mod` factor) == 0

factor :: Integral a => a -> [a]
factor x
    | can_be_factored x = low_factor : factor (quot x low_factor)
    | otherwise = [x]
    where low_factor = (find_lowest_factor x)
