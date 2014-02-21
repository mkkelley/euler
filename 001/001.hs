

mul_three x = (x `mod` 3) == 0
mul_five x = (x `mod` 5) == 0
mul_three_five x = mul_five x || mul_three x

num_if_matches x
    | mul_three_five x = x
    | otherwise = 0
sum_if_matches a b = num_if_matches a + b

sum_if_matches_to_limit x = foldr sum_if_matches 0 [1..x]

main = do
    putStrLn $ show (sum_if_matches_to_limit 999)
