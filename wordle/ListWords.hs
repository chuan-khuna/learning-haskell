module ListWords where

import System.Random

listWords :: [String]
listWords = ["apple", "table", "grape", "music", "jumps", "happy", "quick", "water", "zebra", "bread"]

getRandomWord :: [String] -> IO String
getRandomWord ws = do
    index <- randomRIO (0, length ws - 1)
    return (ws !! index)
