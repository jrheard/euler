-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

import Data.List (nub, permutations)


routes :: Int -> Int -> [[Int]]
routes x y = nub $ permutations $ concat [(take x (repeat 0)), (take y (repeat 1))]

main = print $ length (routes 2 2)
