-- We want to define a data structure for binary trees, called BBtree, where in each node are stored two values of the
-- same type. Write the following:
-- 1. The BBtree data definition.
-- 2. A function bb2list which takes a BBtree and returns a list with the contents of the tree.
-- 3. Make BBtree an instance of Functor and Foldable.
-- 4. Make BBtree an instance of Applicative, using a “zip-like” approach, i.e. every function in the first
-- argument of <*> will be applied only once to the corresponding element in the second argument of <*>.
-- 5. Define a function bbmax, together with its signature, which returns the maximum element stored in the
-- BBtree, if present, or Nothing if the data structure is empty.


data BBtree a = Branch (BBtree a) a a (BBtree a) | NaN deriving (Eq, Show)

bb2list (Branch l v1 v2 r) = bb2list l ++ [v1, v2] ++ bb2list r
bb2list NaN = []

instance Functor BBtree where
    fmap f (Branch l v1 v2 r) = Branch (fmap f l) (f v1) (f v2) (fmap f r)
    fmap f NaN = NaN

instance Foldable BBtree where
    foldr f z (Branch l v1 v2 r) = foldr f (foldr f (foldr f z r) [v1,v2]) l
    foldr _ z NaN = z

instance Applicative BBtree where
    pure x = Branch NaN x x NaN
    (Branch ftl f1 f2 ftr) <*> (Branch l v1 v2 r) = Branch (ftl <*> l) (f1 v1) (f2 v2) (ftr <*> r)
    NaN <*> _ = NaN
    _ <*> NaN = NaN

bbmax :: (Ord a) => BBtree a -> Maybe a
bbmax x@(Branch l v1 v2 r) = Just $ foldr max v1 x
bbmax NaN = Nothing  
