import ConvertEmoji
import Lib
import ListWords

getResult :: String -> String -> String
getResult guess ans = readableState
  where
    state = initState (length guess)
    correctState = setCorrectState guess ans state
    correctQuota = setCorrectQuota guess ans (getQuota ans)
    resultState = setMisplacedState guess correctQuota correctState
    readableState = convertToReadable resultState

-- get the guess input until it is valid
getGuessInput :: Int -> IO String
getGuessInput l = do
    putStrLn ("Please enter your guess(" ++ show l ++ " characters):")
    guess <- getLine
    if length guess /= l
        then do
            getGuessInput l
        else return guess

-- play until the guess is correct
playWordle :: String -> IO ()
playWordle ans = do
    let l = length ans
    guess <- getGuessInput l
    let result = getResult guess ans
    putStrLn result
    if guess == ans
        then putStrLn "You win!"
        else do
            putStrLn (getResult guess ans)
            playWordle ans

main :: IO ()
main = do
    ans <- getRandomWord listWords
    playWordle ans
