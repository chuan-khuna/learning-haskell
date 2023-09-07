import Data.List
import System.Random.Stateful (IOGen)

data WordleState = Wrong | Misplaced | Correct | Undefined

instance Eq WordleState where
    -- Define equality for State values
    (==) :: WordleState -> WordleState -> Bool
    Wrong == Wrong = True
    Misplaced == Misplaced = True
    Correct == Correct = True
    Undefined == Undefined = True
    _ == _ = False

instance Show WordleState where
    show :: WordleState -> String
    show Wrong = "wrong"
    show Misplaced = "misplaced"
    show Correct = "correct"
    show Undefined = "undefined"

initState :: Int -> [WordleState]
initState 0 = []
initState n = Undefined : initState (n - 1)

-- valid the length of guess word must be equal to the answer word
validGuess :: String -> String -> Bool
validGuess guess ans = length guess == length ans

hasQuotaLeft :: Char -> String -> Bool
hasQuotaLeft c quota = c `elem` quota

getQuota :: String -> String
getQuota ans = sort ans

-- for char c in guess
-- remove using quota q:qs if c exists in quota
useQuota :: Char -> String -> String
useQuota _ [] = []
useQuota c (q : qs)
    | c == q = qs
    | otherwise = q : useQuota c qs

setCorrectState :: String -> String -> [WordleState] -> [WordleState]
setCorrectState [] [] [] = []
setCorrectState (g : gs) (a : as) (s : ss)
    | g == a && s == Undefined = Correct : setCorrectState gs as ss
    | otherwise = s : setCorrectState gs as ss

setCorrectQuota :: String -> String -> String -> String
setCorrectQuota [] [] quota = quota
setCorrectQuota (g : gs) (a : as) (q : qs)
    | g == a = setCorrectQuota gs as (useQuota g (q : qs))
    | otherwise = setCorrectQuota gs as (q : qs)

setMisplacedState :: String -> String -> [WordleState] -> [WordleState]
setMisplacedState (g : gs) (q : qs) (s : ss)
    | s == Correct = s : setMisplacedState gs (q : qs) ss
    | hasQuotaLeft g (q : qs) = Misplaced : setMisplacedState gs (useQuota g (q : qs)) ss
    | otherwise = Wrong : setMisplacedState gs (q : qs) ss
setMisplacedState _ _ state = state

convertToReadable :: [WordleState] -> String
convertToReadable [] = []
convertToReadable (s : ss)
    | s == Correct = "🟩" ++ convertToReadable ss
    | s == Misplaced = "🟨" ++ convertToReadable ss
    | s == Wrong = "⬜️" ++ convertToReadable ss

playWordle :: String -> String -> String
playWordle guess ans = readableState
  where
    state = initState (length guess)
    correctState = setCorrectState guess ans state
    correctQuota = setCorrectQuota guess ans (getQuota ans)
    resultState = setMisplacedState guess correctQuota correctState
    readableState = convertToReadable resultState


main :: IO ()
main = do
    putStrLn "Welcome to Wordle! (just compare guess vs answer)"
    putStrLn "Please enter your guess:"
    guess <- getLine
    putStrLn "Please enter the answer:"
    ans <- getLine
    putStrLn (playWordle guess ans)

-- guess = "cat"
-- ans = "act"

-- state = initState (length guess)
-- correctState = setCorrectState guess ans state
-- correctQuota = setCorrectQuota guess ans (getQuota ans)
-- resultState = setMisplacedState guess correctQuota correctState
