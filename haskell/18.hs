-- Find the maximum total from top to bottom of the triangle.
import Data.List
import Data.List.Split

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeLevels = splitOn "," "75,95 64,17 47 82,18 35 87 10,20 04 82 47 65,19 01 23 75 03 34,88 02 77 73 07 63 67,99 65 04 28 06 16 70 92,41 41 26 56 83 40 80 70 33,41 48 72 33 47 32 37 16 94 29,53 71 44 65 25 43 91 52 97 51 14,70 11 33 28 77 73 17 78 39 68 17 57,91 71 52 38 17 14 91 43 58 50 27 29 48,63 66 04 68 89 53 67 30 73 16 69 87 40 31,04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

singleton :: Int -> Tree
singleton x = Node x EmptyTree EmptyTree

treeLength :: Tree -> Int
treeLength EmptyTree = 0
treeLength Node a left right = 1 + (treeLength left) + (treeLength right)

treeAppend :: Int -> Tree -> Tree
treeAppend x EmptyTree = Node x EmptyTree EmptyTree
treeAppend x tree = treeInsertAt x (treeLength tree) tree

treeLevelThresholds :: [Int]
treeLevelThresholds = 0 : zipWith (+) treeLevelThresholds [1..] -- [0,1,3,6,10,15...]

treeLevel :: Int -> Int
treeLevel 0 = 0
treeLevel len = case (findIndex (\threshold -> threshold > len) treeLevelThresholds) of
    Just level -> level - 1
    Nothing -> 0

treeInsertAt :: Int -> Int -> Int -> Int -> Tree -> Tree
treeInsertAt x _ _ _ EmptyTree = singleton x
treeInsertAt x pos tree = insert x pos 0 0 tree where
    insert x pos currentLevel currentOffset (Node a left right)
        | destinationOffset == 0 = let
            tree | currentLevel == (destinationLevel - 1) = Node a (singleton x) right
                 | otherwise = Node a (insert x pos (currentLevel + 1) 0 tree) right
            in tree
        | destinationOffset == destinationLevel = let
            tree | currentLevel == (destinationLevel - 1) = Node a left (singleton x)
                 | otherwise = Node a left (insert x pos (currentLevel + 1) (currentOffset + 1) tree)
            in tree
        | otherwise = Node a (insert x pos (currentLevel + 1) foo tree) (bar) -- TODO jrheard
        where destinationLevel = treeLevel $ treeLength tree
              destinationOffest = pos - (treeLevelThresholds !! destinationLevel)



makeTree :: [String] -> Tree -- implement using a fold using treeAppend



main = print "hi"
