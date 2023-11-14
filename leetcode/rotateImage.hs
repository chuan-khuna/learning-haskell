-- leetcode 48 Rotate Image: https://leetcode.com/problems/rotate-image/description/

import Data.List as L

rotateImage :: [[Int]] -> [[Int]]
rotateImage [[]] = [[]]
rotateImage img = map reverse (L.transpose img)
