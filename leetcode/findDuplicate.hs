-- Find all duplicate numbers in an array with multiple duplicates 
-- Return: array — containing all duplicates found or empty array if none are found 

--  returnMultipleDupesArray = (arr) => { // // }; 
-- const arr = [1, 1, 2, 3, 4, 5, 6, 7, 8, 6, 6, 7, 7, 7, 10, 10]; 
-- console.log(returnMultipleDupesArray(arr)); // prints [1, 6, 7, 10]

-- https://leetcode.com/problems/find-all-duplicates-in-an-array/description/

import Data.Map as M

findDuplicate :: [Int] -> [Int]
findDuplicate [] = []
findDuplicate (x:xs)
    | x `elem` xs = nub (x : findDuplicate xs)
    | otherwise = nub (findDuplicate xs)


findDup :: [Int] -> [Int] -> [Int]
findDup [] dupList = dupList
findDup (x:xs) dupList
    | (x `elem` xs) && notElem x dupList = findDup xs (x:dupList)
    | otherwise = findDup xs dupList

findDupMap :: [Int] -> [Int]
findDupMap arr = L.map (\(x, y) -> x) $ L.filter (\(x, y) -> y > 1) $ M.toList map
    where
        createMap :: [Int] -> Map Int Int -> Map Int Int
        createMap [] map = map
        createMap (x:xs) map
            | M.lookup x map == Nothing = createMap xs (M.insert x 1 map)
            | otherwise = createMap xs (M.adjust (+1) x map)
        map = createMap arr M.empty