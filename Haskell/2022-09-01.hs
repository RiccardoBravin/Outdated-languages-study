-- We want to implement a binary tree where in each node is stored data, together with the number of nodes
-- contained in the subtree of which the current node is root.
-- 1. Define the data structure.
-- 2. Make it an instance of Functor, Foldable, and Applicative.

data Btree a = Btree a (Btree a) (Btree a) Int | NaN deriving (Eq, Show)--not a node

instance Functor Btree where
    fmap f NaN = NaN
    fmap f (Btree a l r count) = Btree (f a) (fmap f l) (fmap f r) count

instance Foldable Btree where
    foldr _ z NaN = z
    foldr f z (Btree a l r _) = foldr f (foldr f (f a z) r) l

x +++ NaN = x
NaN +++ x = x
(Btree a l r count) +++ (Btree a' l' r' count') = Btree a l (r +++ (Btree a' l' r' count')) (count + count')

btconcat x = foldr (+++) NaN x
btconcatmap f tree = btconcat (fmap f tree)

instance Applicative Btree where
    pure x = Btree x NaN NaN 0
    fbt <*> lbt = btconcatmap (\f -> fmap f lbt) fbt