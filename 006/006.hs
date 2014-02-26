
{-
 - Euler problem #6 is asking for this:
 -  100       100
 -  (E i)^2 -  E (i^2)
 -  i=1       i=1
 -
 -  where E is the summation operator.
 -  Brute forcing this is a bad idea.
 -  Mathematically expanding them:
 -
 -  (n(n+1)/2)^2 - (n(n+1)(2n+1)/6)
 -  
 -  Is a nice, simple calculation. Do the algebra
 -  to get to the version seen in the code.
 -}

sqsum_minus_sumsq_from_1_to n =
    n^4/4 + n^3/6 - n^2/4 - n/6

main = do
    putStrLn $ show $ sqsum_minus_sumsq_from_1_to 100
