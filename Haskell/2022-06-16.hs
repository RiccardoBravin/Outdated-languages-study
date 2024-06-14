-- Consider the "fancy pair" data type (called Fpair), which encodes a pair of the same type a, and may
-- optionally have another component of some "showable" type b, e.g. the character '$'.
-- Define Fpair, parametric with respect to both a and b.
-- 1) Make Fpair an instance of Show, where the implementation of show of a fancy pair e.g. encoding
-- (x, y, '$') must return the string "[x$y]", where x is the string representation of x and y of y. If the third
-- component is not available, the standard representation is "[x, y]".
-- 2) Make Fpair an instance of Eq — of course the component of type b does not influence the actual
-- value, being only part of the representation, so pairs with different representations could be equal.
-- 3) Make Fpair an instance of Functor, Applicative and Foldable



data Fpair s a = Fpair a a s | Pair a a

instance (Show a, Show s) => Show (Fpair s a) where
    show (Fpair v1 v2 c) = "[" ++ (show v1) ++ (show c) ++ (show v2) ++ "]"
    show (Pair v1 v2) = "[" ++ (show v1) ++ "," ++ (show v2) ++ "]"

instance (Eq b) => Eq (Fpair a b) where
    Fpair v1 v2 _ == Fpair v1' v2' _ = (v1 == v1') && (v2 == v2')
    Pair v1 v2 == Pair v1' v2' = (v1 == v1') && (v2 == v2')

instance Functor (Fpair x) where
    fmap f (Fpair v1 v2 b) = Fpair (f v1) (f v2) b
    fmap f (Pair v1 v2) = Pair (f v1) (f v2)

instance Foldable (Fpair s) where
    foldr f z (Fpair v1 v2 _) = f v1 (f v2 z)

instance Applicative (Fpair s) where
    pure x = Pair x x
    (Fpair f g _) <*> (Fpair x y v) = Fpair (f x) (g y) v
    (Pair f g) <*> (Fpair x y v) = Fpair (f x) (g y) v
    (Fpair f g _) <*> (Pair x y) = Pair (f x) (g y)
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y) 

