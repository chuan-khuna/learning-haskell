isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x : xs) = (x == last xs) && isPalindrome middle
  where
    middle = init (tail (x : xs))
