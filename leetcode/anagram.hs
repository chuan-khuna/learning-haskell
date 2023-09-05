import Data.List

isAnagram :: String -> String -> Bool
isAnagram x y = sort x == sort y
