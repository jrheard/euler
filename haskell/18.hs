-- Find the maximum total from top to bottom of the triangle.
import Data.List

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeSpec = map (\x -> (read x) :: Int) $ words "75 95 64 17 47 82 18 35 87 10 20 04 82 47 65 19 01 23 75 03 34 88 02 77 73 07 63 67 99 65 04 28 06 16 70 92 41 41 26 56 83 40 80 70 33 41 48 72 33 47 32 37 16 94 29 53 71 44 65 25 43 91 52 97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91 71 52 38 17 14 91 43 58 50 27 29 48 63 66 04 68 89 53 67 30 73 16 69 87 40 31 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeLength :: Tree a -> Int
treeLength EmptyTree = 0
treeLength (Node a left right) = 1 + (treeLength left) + (treeLength right)

treeAppend :: a -> Tree a -> Tree a
treeAppend x EmptyTree = Node x EmptyTree EmptyTree
treeAppend x tree = treeInsertAt x (treeLength tree) tree

treeLevelThresholds :: [Int]
treeLevelThresholds = 0 : zipWith (+) treeLevelThresholds [1..] -- [0,1,3,6,10,15...]

treeLevel :: Int -> Int
treeLevel 0 = 0
treeLevel len = case (findIndex (\threshold -> threshold > len) treeLevelThresholds) of
    Just level -> level - 1
    Nothing -> 0

treeInsertAt :: a -> Int -> Tree a -> Tree a
treeInsertAt x _ EmptyTree = singleton x
treeInsertAt x pos (Node a left right)
    | pos == 1 = Node a (singleton x) right
    | pos == 2 = Node a left (singleton x)
    | pos == 4 = Node a (treeInsertAt x 2 left) (treeInsertAt x 1 right)
    | destinationOffset < (numNodesInDestinationLevel `quot` 2) = Node a (treeInsertAt x (pos - destinationLevel) left) right
    | otherwise = Node a left (treeInsertAt x (pos - destinationLevel - 1) right)
    where destinationLevel = treeLevel $ treeLength (Node a left right)
          destinationOffset = pos - (treeLevelThresholds !! destinationLevel)
          numNodesInDestinationLevel = (treeLevelThresholds !! (destinationLevel + 1)) - (treeLevelThresholds !! destinationLevel)

makeTree :: [a] -> Tree a
makeTree treeSpec = foldl (\acc x -> treeAppend x acc) EmptyTree treeSpec



main = print "hi"
