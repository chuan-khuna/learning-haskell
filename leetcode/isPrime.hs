intsqrt :: Int -> Int
intsqrt n = floor (sqrt (fromIntegral n))

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all (\x -> n `mod` x /= 0) [2 .. intsqrt n]
