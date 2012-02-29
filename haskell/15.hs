-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

import Data.List (delete, nub, permutations)


data Direction = D | R deriving (Eq, Show)


routes :: Int -> Int -> [[Int]]
routes x y = nub $ permutations $ concat [(take x (repeat 0)), (take y (repeat 1))]

routesLeft :: [Direction] -> [[Direction]]
routesLeft remaining
    | D `elem` remaining && R `elem` remaining = concat [downBranch, rightBranch]
    | D `elem` remaining = downBranch
    | R `elem` remaining = rightBranch
    | otherwise = [[]]
    where downBranch = [D:route | route <- routesLeft (delete D remaining)]
          rightBranch = [R:route | route <- routesLeft (delete R remaining)]

routes' :: Int -> Int -> [[Direction]]
routes' x y = routesLeft initialDirections where
    initialDirections = concat [(take x (repeat R)), (take y (repeat D))]

main = print $ length (routes' 20 20)
