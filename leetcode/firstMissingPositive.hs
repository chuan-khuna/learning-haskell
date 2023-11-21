-- https://leetcode.com/problems/search-insert-position/description/

replaceNegative :: [Int] -> [Int]
replaceNegative [] = []
replaceNegative (x : xs)
    | x < 0 = 0 : replaceNegative xs
    | otherwise = x : replaceNegative xs

-- optimise this to hash on itself?
hashArr :: [Int] -> [Int] -> [Int]
-- arr, indexArr(= arr) -> return hashedArr
hashArr arr [] = arr
hashArr arr (x : xs) = hashArr (hashAtIndex arr x) xs
  where
    hashAtIndex :: [Int] -> Int -> [Int]
    -- arr, index -> return hashedArr
    -- replace arr[index] with negative value to encode information
    -- where `index` is (value - 1)
    -- eg value 2 -> set arr[1] to negative value
    hashAtIndex arr index
        -- index out of range (from default negative), index = 0 -> do nothing
        | abs index > (length arr - 1) || index == 0 = arr
        -- assign negative w/ default value (larger than arr size)
        | y == 0 = x ++ [-(length arr + 1)] ++ ys
        | y > 0 = x ++ [-y] ++ ys
        | otherwise = x ++ [y] ++ ys
      where
        -- y is the value to be replaced
        (x, y : ys) = splitAt (abs index - 1) arr

solve :: [Int] -> Int
solve arr = findMissingInt hashedArr [1 .. length arr]
  where
    cleanedNegArr = replaceNegative arr
    hashedArr = hashArr cleanedNegArr cleanedNegArr

    findMissingInt :: [Int] -> [Int] -> Int
    findMissingInt [] [] = 1
    findMissingInt hashedArr (i : is)
        | hashedArr !! (i - 1) >= 0 = i
        | otherwise = findMissingInt hashedArr is
