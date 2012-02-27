-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

import Data.List (delete, nub, permutations)


data Direction = D | R


routes :: Int -> Int -> [[Int]]
routes x y = nub $ permutations $ concat [(take x (repeat 0)), (take y (repeat 1))]

routes' :: Int -> Int -> [[Direction]]
routes' x y = routes' initialDirections where
    routes' remaining
        | D `elem` remaining && R `elem` remaining = [downBranch, rightBranch]
        | D `elem` remaining = downBranch -- the issue here is that some branches are returning [Direction]s but others are returning [[Direction]]s.
        | R `elem` remaining = rightBranch
        | otherwise = []
        where downBranch = D : (concat $ routes' (delete D remaining))
              rightBranch = R : (concat $ routes' (delete R remaining))
    initialDirections = concat [(take x (repeat R)), (take y (repeat D))]

main = print $ length (routes 2 2)
