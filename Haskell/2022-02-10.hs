-- Consider a data structure Gtree for general trees, i.e. trees containg some data in each node, and a
-- variable number of children.
-- 1. Define the Gtree data structure.
-- 2. Define gtree2list, i.e. a function which translates a Gtree to a list.
-- 3. Make Gtree an instance of Functor, Foldable, and Applicative.

data Gtree a = Gtree a [Gtree a] | NaN deriving Show-- Node with value and list of trees or Not a Node

gtree2list NaN = []
gtree2list (Gtree v l) = v : (foldr (++) [] (fmap gtree2list l))
--gtree2list (Gtree v l) = v : concatMap gtree2list l --a shorter and more understandable way to do it


instance Functor Gtree where
    fmap _ NaN = NaN
    fmap f (Gtree v l) = Gtree (f v) (fmap (fmap f) l) --do fmap f to each element of the list l

instance Foldable Gtree where
    foldr f z t = foldr f z (gtree2list t)

NaN +++ x = x
x +++ NaN = x
(Gtree v l) +++ (Gtree v' l') = Gtree v (l ++ [Gtree v' l'])

gtConcat tree = foldr (+++) NaN tree

gtConcatMap f tree = gtConcat (fmap f tree)

instance Applicative Gtree where
    pure v = Gtree v []
    x <*> y = gtConcatMap (\f -> fmap f y) x 


-- main to test all the functions
main = do
    print $ gtree2list (Gtree 1 [Gtree 2 [], Gtree 3 [Gtree 4 [], Gtree 5 []]])
    print $ fmap (+1) (Gtree 1 [Gtree 2 [], Gtree 3 [Gtree 4 [], Gtree 5 []]])
    print $ foldr (+) 0 (Gtree 1 [Gtree 2 [], Gtree 3 [Gtree 4 [], Gtree 5 []]])
    print $ (Gtree (+1) [Gtree (+2) [], Gtree (* 1) []]) <*> (Gtree 1 [Gtree 2 [], Gtree 3 [Gtree 4 [], Gtree 5 []]])

