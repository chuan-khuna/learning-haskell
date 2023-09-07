module ConvertEmoji where

import Lib

-- this function cannot be formatted by ormolu, I separate it from the other code
convertToReadable :: [WordleState] -> String
convertToReadable [] = []
convertToReadable (s : ss)
    | s == Correct = "🟩" ++ convertToReadable ss
    | s == Misplaced = "🟨" ++ convertToReadable ss
    | s == Wrong = "⬜️" ++ convertToReadable ss
    | otherwise = "🔥" ++ convertToReadable ss