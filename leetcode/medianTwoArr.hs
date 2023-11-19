-- https://leetcode.com/problems/median-of-two-sorted-arrays/description/

medianSearch :: [Int] -> [Int] -> Int -> Int -> Double
-- shortArr, longArr, shortArrLeft, shortArrRight -> return median
medianSearch shortArr longArr leftIdx rightIdx
    | correctPartitioned = median
    | otherwise = medianSearch shortArr longArr newLeft newRight
  where
    inf = read "Infinity" :: Double
    totalLength = length shortArr + length longArr
    half = totalLength `div` 2

    -- middleIdx of short array
    middleIdx = (leftIdx + rightIdx) `div` 2
    -- middleIdx of long array
    longArrIdx = half - (middleIdx + 1) - 1

    -- get left-right value from both arrays
    shortLeft = if middleIdx >= 0 then fromIntegral (shortArr !! middleIdx) :: Double else -inf
    shortRight = if middleIdx + 1 < length shortArr then fromIntegral (shortArr !! (middleIdx + 1)) :: Double else inf
    longLeft = if longArrIdx >= 0 then fromIntegral (longArr !! longArrIdx) :: Double else -inf
    longRight = if longArrIdx + 1 < length longArr then fromIntegral (longArr !! (longArrIdx + 1)) :: Double else inf

    -- check if partitioned correctly
    correctPartitioned = shortLeft <= longRight && longLeft <= shortRight

    median =
        if even totalLength
            then (max shortLeft longLeft + min shortRight longRight) / 2
            else min shortRight longRight

    newLeft = if shortRight < longLeft then middleIdx + 1 else leftIdx
    newRight = if shortLeft > longRight then middleIdx - 1 else rightIdx

solve :: [Int] -> [Int] -> Double
solve nums1 nums2
    -- swap if nums1 is longer than nums2
    | length nums1 > length nums2 = solve nums2 nums1
    | otherwise = medianSearch nums1 nums2 0 (length nums1 - 1)

nums1 = [1, 2]
nums2 = [3, 4]

solve nums1 nums2
