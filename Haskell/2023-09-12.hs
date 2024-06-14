-- Consider the binary tree data structure as seen in class.
-- 1. Define a function btrees which takes a value x and returns an infinite list of binary trees, where:
--      1. all the leaves contain x,
--      2. each tree is complete,
--      3. the first tree is a single leaf, and each tree has one level more than its previous one in the list.
-- 2. Define an infinite list of binary trees, which is like the previous one, but the first leaf contains the integer 1,
-- and each subsequent tree contains leaves that have the value of the previous one incremented by one.
-- E.g. [Leaf 1, (Branch (Leaf 2)(Leaf 2), ...]
-- 3. Define an infinite list containing the count of nodes of the trees in the infinite list of the previous point.
-- E.g. [1, 3, ...]
-- Write the signatures of all the functions you define

data Btree a = Leaf a | Branch (Btree a) (Btree a) deriving (Show)

deepen::Btree a -> Btree a
deepen (Leaf x) = Branch (Leaf x) (Leaf x)
deepen (Branch l r) = Branch (deepen l) (deepen r)

btrees:: t -> [Btree t]
btrees x = Leaf x : [deepen l | l <- btrees x]

deepenAdd::Btree Int -> Btree Int
deepenAdd (Leaf x) = Branch (Leaf (x + 1)) (Leaf (x + 1))
deepenAdd (Branch l r) = Branch (deepenAdd l) (deepenAdd r)

infBtreeScale::[Btree Int]
infBtreeScale =  Leaf 1 : [deepenAdd l | l <- infBtreeScale]

cntNodes::[Int]
cntNodes = 1 : [(x+1) * 2 - 1 | x <- cntNodes]
