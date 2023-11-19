-- https://leetcode.com/problems/search-insert-position/description/

binarySearch :: (Ord a) => [a] -> a -> Int
-- arr value -> index
-- assisted by copilot
binarySearch arr value = binarySearch' arr value 0 (length arr - 1)
  where
    binarySearch' arr value left right
        | left > right = left
        | otherwise =
            let mid = (left + right) `div` 2
             in case compare (arr !! mid) value of
                    -- arr[mid] < value
                    LT -> binarySearch' arr value (mid + 1) right
                    EQ -> mid
                    -- arr[mid] > value
                    GT -> binarySearch' arr value left (mid - 1)

binarySearch [1, 3, 5, 6] (-1)
