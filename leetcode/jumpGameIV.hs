-- https://leetcode.com/problems/jump-game-iv/description/

import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set

generateMap :: [Int] -> Map.Map Int [Int]
-- arr -> [(val, [indices])]
generateMap arr = Map.fromListWith (++) $ zip arr [[i] | i <- [0 ..]]

getNeighbours :: [Int] -> Int -> Map.Map Int [Int] -> [Int]
-- arr, index, routeMap -> [neighbour index]
getNeighbours [] _ _ = []
getNeighbours arr index routeMap = neighbours
  where
    val = arr !! index
    warpNeighbours = List.delete index $ Maybe.fromJust (Map.lookup val routeMap)
    neighbours = List.filter (< length arr) $ List.filter (> 0) $ warpNeighbours ++ [index - 1, index + 1]

bfs :: [Int] -> Map.Map Int [Int] -> [(Int, [Int])] -> [Int] -> [(Int, [Int])]
-- bfs arr routeMap queue visited -> [(distance, nodes)]
-- queue: [(next distance, next nodes)]
bfs arr routeMap queue visited
    | Set.fromList visited == Set.fromList [0 .. length arr - 1] = queue
    | otherwise = bfs arr routeMap newQueue newVisited
  where
    -- current visited
    (distance, nodes) = last queue
    -- get all neighbours that can be visited by current nodes
    neighbours = nub $ List.concatMap (\index -> getNeighbours arr index routeMap) nodes
    newQueue = queue ++ [(distance + 1, [i | i <- neighbours, i `notElem` visited])]
    newVisited = nub (visited ++ neighbours)

solve :: [Int] -> Int
solve [] = 0
solve [x] = 0
solve arr = distance
  where
    routeMap = generateMap arr
    traveralResult = bfs arr routeMap [(0, [0])] [0]
    (distance, nodes) = head $ List.filter (\(dist, nodes) -> length arr - 1 `elem` nodes) traveralResult
