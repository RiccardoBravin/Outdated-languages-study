-- Define a data structure for Binary Search Trees (BST), i.e. ordered trees where
-- elements less than the value stored in the current node are in the left subtree,
-- while elements greater than or equal to it are in the right subtree. Define a
-- put operation to put a value in a BST, and a member operation to check if a
-- value is present or not. Provide all the types of the defined operations.



data BST a = Branch (BST a) a (BST a) | NaN


insert :: Ord t => t -> BST t -> BST t
insert x NaN = Branch NaN x NaN
insert x (Branch l a r) = if (x >= a) then insert x r else insert x l

search :: Ord t => t -> BST t -> Bool
search x (Branch l a r)
  | x == a = True
  | x >= a = search x r
  | otherwise = search x l

search x NaN = False