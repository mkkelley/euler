import Data.List
import Data.Char

splitWhen p s = case dropWhile p s of
                "" -> []
                s' -> w : splitWhen p s''
                    where (w, s'') = break p s'
                    
splitOn sep s = splitWhen (==sep) s

readAndSplit :: String -> IO [String]
readAndSplit filename = do
    text <- readFile filename
    return (splitOn ',' text)

stripQuotes :: String -> String
stripQuotes s = (tail (init s))

aValue [] = 0
aValue name = (ord $ head name) - (ord 'A' - 1) + (aValue $ tail name)

scoreNames [] _ = 0
scoreNames names n = (aValue $ head names) * n +
                        (scoreNames (tail names) (n+1))

stripAndSort x = sort $ map stripQuotes x

main = do 
    x <- readAndSplit "names.txt"
    putStrLn $ show (scoreNames (stripAndSort x) 1)
