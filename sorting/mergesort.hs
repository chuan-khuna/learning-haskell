-- Merge two sorted lists into a single sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

-- Merge sort implementation
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
    let (left, right) = splitAt (length xs `div` 2) xs
     in merge (mergesort left) (mergesort right)

main :: IO ()
main = do
    let inputList = reverse [1 .. 10]
    let result = mergesort inputList
    putStrLn ("Input: " ++ show inputList)
    putStrLn ("Output: " ++ show result)
