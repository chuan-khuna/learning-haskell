-- https://www.youtube.com/watch?v=rIprO6zoujM (~25:00)
-- Quick sort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = quicksort ys ++ [x] ++ quicksort zs
  where
    -- everything smaller than x
    ys = [a | a <- xs, a <= x]
    -- everything greater than x
    zs = [b | b <- xs, b > x]

main :: IO ()
main = do
    let inputList = [1, 3, 4, 2]
    let result = quicksort inputList
    putStrLn ("Input: " ++ show inputList)
    putStrLn ("Output: " ++ show result)
