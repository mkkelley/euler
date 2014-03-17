
factorial n = product [1..n]

digit_sum 0 = 0
digit_sum n = (mod n 10) + (digit_sum (quot n 10))

main = do
    putStrLn $ show $ digit_sum $ factorial 100
