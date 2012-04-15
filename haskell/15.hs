-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

gridCounts x y = counts where
    counts = [[gridVal xx yy | yy <- [0..y-1]] | xx <- [0..x-1]]
    gridVal x y
        | x == 0 && y == 0 = 2
        | x == 0 || y == 0 = 1
        | otherwise = ((counts !! (x - 1)) !! y) + ((counts !! x) !! (y - 1))

numRoutes :: Int -> Int -> Int
numRoutes x y = sum $ map (\x -> sum x) (gridCounts x y)

main = print $ numRoutes 20 20
