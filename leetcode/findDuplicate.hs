-- Find all duplicate numbers in an array with multiple duplicates 
-- Return: array — containing all duplicates found or empty array if none are found 

--  returnMultipleDupesArray = (arr) => { // // }; 
-- const arr = [1, 1, 2, 3, 4, 5, 6, 7, 8, 6, 6, 7, 7, 7, 10, 10]; 
-- console.log(returnMultipleDupesArray(arr)); // prints [1, 6, 7, 10]

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