import System.Win32 (xBUTTON1)
-- 1. Define a data structure, called D2L, to store lists of possibly depth two, e.g. like [1,2,[3,4],5,[6]].
-- 2. Implement a flatten function which takes a D2L and returns a flat list containing all the stored values in it
-- in the same order.
-- 3. Make D2L an instance of Functor, Foldable, Applicative.

data D2L a = NaL | D2L1 a (D2L a) | D2L2 [a] (D2L a) deriving (Show, Eq)

flatten NaL = []
flatten (D2L1 v c) = v : flatten c
flatten (D2L2 l c) = l ++ flatten c

instance Functor D2L where
    fmap f NaL = NaL
    fmap f (D2L1 v c) = D2L1 (f v) (fmap f c)
    fmap f (D2L2 l c) = D2L2 (fmap f l) (fmap f c)

instance Foldable D2L where
    foldr f z NaL = z
    foldr f z (D2L1 v c) = f v (foldr f z c)
    foldr f z (D2L2 l c) = foldr f (foldr f z c) l

NaL +++ x = x
x +++ NaL = x
(D2L1 v c) +++ x = D2L1 v (c +++ x)
(D2L2 l c) +++ x = D2L2 l (c +++ x)

d2lconcat lst = foldr (+++) Nal lst
d2lconcatmap f list = d2lconcat (fmap f list)


instance Applicative D2L where
    pure v = D2L1 v NaL
    fs <*> vs = d2lconcatmap (\f -> fmap f vs) fs