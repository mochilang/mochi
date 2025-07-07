module Main where

data Tree = Leaf | Node Tree Int Tree

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node l v r) = treeSum l + v + treeSum r

example :: Tree
example = Node Leaf 1 (Node Leaf 2 Leaf)

main :: IO ()
main = print (treeSum example)
