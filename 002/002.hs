
iter_fib _ b 1 = b
iter_fib a b countdown = iter_fib b (a+b) (countdown-1)

fib n = iter_fib 1 1 n

rfib_list 0 = []
rfib_list n = (iter_fib 1 1 n) : (rfib_list (n-1))
fib_list n = reverse (rfib_list n)

-- fib 32 is the last one before 4000000
even_fib_list n = filter even (fib_list n)

main = do
    putStrLn $ show $ foldr (+) 0 (even_fib_list 32)
