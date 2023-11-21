-- https://leetcode.com/problems/minimum-amount-of-time-to-collect-garbage/description/

collectTime :: [String] -> [Int] -> Char -> Int
collectTime garbage travel garbageType = collectTime' (reverse garbage) (reverse travel) garbageType
  where
    -- houses, travel time, garbage type -> total time
    -- but we need to run this function in reverse order
    collectTime' :: [String] -> [Int] -> Char -> Int
    collectTime' [] _ _ = 0
    collectTime' (h : hs) (t : ts) g
        | null filteredGarbage = collectTime' hs ts g
        | otherwise = sum (t : ts) + sum countGarbage
      where
        filteredGarbage = filter (== g) h
        countGarbage = map (length . filter (== g)) (h : hs)

garbage = ["MMM", "PGM", "GP"]
travel = [3, 10]
sum (map (collectTime garbage travel) "GPM")


-- but what is Eta reduce?
-- collectTime garbage travel = collectTime' (reverse garbage) (reverse travel)