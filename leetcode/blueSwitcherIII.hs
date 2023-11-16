-- https://leetcode.ca/all/1375.html
-- https://beizhedenglong.github.io/leetcode-solutions/docs/bulb-switcher-iii
-- https://leetcode.com/problems/bulb-switcher-ii/

isBlue :: [Int] -> Int -> Bool
-- isBlue lights k
-- at the kth turn, are all lights blue?
-- k: 0-indexed id that turn on the light [k]
isBlue lights k = maximum (take (k + 1) lights) <= k + 1

getBlueStatus :: [Int] -> [Bool]
getBlueStatus lights = map (isBlue lights) indices
  where
    indices = [0 .. length lights - 1]

countBlueStatus :: [Int] -> Int
countBlueStatus lights = length $ filter id $ getBlueStatus lights

lights = [2, 1, 3, 5, 4]
countBlueStatus lights

countBlue' :: [Int] -> Int
countBlue' lights = length $ filter id $ map (isBlue lights) [0 .. length lights - 1]
  where
    isBlue :: [Int] -> Int -> Bool
    isBlue lights k = maximum (take (k + 1) lights) <= k + 1
