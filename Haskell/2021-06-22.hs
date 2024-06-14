-- Define a data-type called BTT which implements trees that can be binary or ternary, and where every
-- node contains a value, but the empty tree (Nil). Note: there must not be unary nodes, like leaves.
-- 1) Make BTT an instance of Functor and Foldable.
-- 2) Define a concatenation for BTT, with the following constraints:
-- • If one of the operands is a binary node, such node must become ternary, and the other operand
-- will become the added subtree (e.g. if the binary node is the left operand, the rightmost node of
-- the new ternary node will be the right operand).
-- • If both the operands are ternary nodes, the right operand must be appened on the right of the left
-- operand, by recursively calling concatenation.
-- 3) Make BTT an instance of Applicative.

data BTT a = Nil | BB a (BTT a) (BTT a) | TB a (BTT a) (BTT a) (BTT a)

instance Show a => Show (BTT a) where
    -- implementation of the show function for BTT goes here
    show Nil = ""
    show (BB v sx dx) = "(" ++ show v ++ " " ++ show sx ++ " " ++ show dx ++ ")"
    show (TB v sx cx dx) = "(" ++ show v ++ " " ++ show sx ++ " " ++ show cx ++ " " ++ show dx ++ ")"

instance Functor BTT where
    fmap _ Nil = Nil
    fmap f (BB v sx dx) = BB (f v) (fmap f sx) (fmap f dx)
    fmap f (TB v sx cx dx) = TB (f v) (fmap f sx) (fmap f cx) (fmap f dx)

instance Foldable BTT where
    foldr _ z Nil = z
    foldr f z (BB a sx dx) = foldr f (f a (foldr f z dx)) sx
    foldr f z (TB a sx cx dx) = foldr f (f a (foldr f (foldr f z dx) cx)) sx


-- there is no standard way to make a thing concatenable so we use <++> as a custom symbol i guess??
x <++> Nil = x
Nil <++> x = x
(BB v dx sx) <++> x = TB v dx sx x
x <++> (BB v dx sx) = TB v x dx sx 
TB v dx cx sx <++> lo@(TB v' dx' cx' sx') = TB v dx cx (sx <++> lo) 

--To implement applicative we need to be able to concatenate trees from a list
--tl is treelist 
tlconcat tl = foldr (<++>) Nil tl --apply the previously defined concatenation recursively on the list of trees
--we also need to be able to apply a function to all elements (the trees) of the list 
tlconcmap f lt = tlconcat (fmap f lt)

instance Applicative BTT where
    pure x = BB x Nil Nil
    tree_of_f <*> tree = tlconcmap (\f -> fmap f tree) tree_of_f



treeInst = BB 8 (BB 5 Nil (BB 0 Nil Nil)) (TB 10 (BB 9 Nil Nil) (BB 10 Nil Nil) (TB 11 Nil Nil Nil))

main = do
    print $ treeInst 
    print $ fmap (+ 1) treeInst
    print $ foldr (+) 0 treeInst 
    print $ treeInst <++> treeInst
    print $ (BB (+ 1) (BB (*2) (Nil) Nil) Nil) <*> treeInst

