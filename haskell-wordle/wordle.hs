check :: String -> String -> String
check [] [] = []
check (x : xs) (y : ys)
    | x == y = 'o' : check xs ys
    | otherwise = 'x' : check xs ys

main :: IO ()
main = do
    let word1 = "hello"
    let word2 = "world"
    putStrLn (check word1 word2)
