data Tree a = EmptyTree | Node a (Tree a) (Tree a)

tree1 :: Tree Int
tree1 = Node 1 (Node 2 EmptyTree (Node 3 EmptyTree EmptyTree)) (Node 2 EmptyTree EmptyTree) 

-- depth of tree is either zero (if empty) or 1 + the depth of the deeper child tree
depth :: (Ord a) => Tree a -> Int
depth EmptyTree = 0
depth (Node x left right) = 
	1 + max (depth left) (depth right)