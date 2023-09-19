-- https://www.youtube.com/watch?v=_DaTsI42Wvo
-- codeforce 1552F. Telepanting Ant: https://codeforces.com/problemset/problem/1552/F

import Data.List

distance :: Int -> Int -> Int
distance x y = abs (x - y)

-- cost = in->out portal distance + (sum of costs of all portals between this in and out portal)
getWarpCost :: [Int] -> [Int] -> [Int]
getWarpCost [] [] = [0]
getWarpCost (x : xs) (y : ys) = (dist + sum (getWarpCost prevInPoints prevOutPoints)) : getWarpCost xs ys
  where
    prevInPoints = filter (< x) (filter (> y) (x : xs))
    prevOutPoints = filter (< x) (filter (> y) (y : ys))
    dist = distance y x

solve :: [Int] -> [Int] -> [Int] -> Int
solve inPoints outPoints states = sum costs_states + last inPoints + 1
  where
    costs = getWarpCost (reverse inPoints) (reverse outPoints)
    costs_states = zipWith (*) costs (reverse states ++ [0])
