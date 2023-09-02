rob :: [Int] -> Int
rob [] = 0
rob [x] = x
rob nums = max (last nums + rob (init (init nums))) (rob (init nums))

main :: IO ()
main = do
    let inputList = [183, 219, 57, 193, 94, 233, 202, 154, 65, 240, 97, 234, 100, 249, 186, 66, 90, 238, 168, 128, 177, 235, 50, 81, 185, 165, 217, 207, 88, 80, 112, 78, 135, 62, 228, 247, 211]

    -- let inputList = [1, 2, 3, 1]
    let result = rob inputList
    putStrLn ("Input: " ++ show inputList)
    putStrLn ("Output: " ++ show result)
