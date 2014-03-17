
divisors n = [x | x <- [1..(n-1)], rem n x == 0]

is_amicable n = sum(divisors (sum $ divisors n)) == n && n/= (sum $ divisors n)

main = do
    putStrLn $ show $ sum [x | x <- [1..10000], is_amicable x]
