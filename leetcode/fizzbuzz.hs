fizzbuzz :: Int -> String
fizzbuzz x
    | x `mod` 15 == 0 = "FizzBuzz"
    | x `mod` 3 == 0 = "Fizz"
    | x `mod` 5 == 0 = "Buzz"
    | otherwise = show x

main :: IO ()
main = do
    let inputList = [1 .. 30]
    let result = map fizzbuzz inputList
    putStrLn ("Input: " ++ show inputList)
    putStrLn ("Output: " ++ show result)
