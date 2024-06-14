-- Consider a Tvtl (two-values/two-lists) data structure, which can store either two values of a given type, or
-- two lists of the same type.
-- Define the Tvtl data structure, and make it an instance of Functor, Foldable, and Applicative.

data Tvtl a = Tl [a] [a] | Tv a a deriving (Show, Eq)

instance Functor Tvtl where
    fmap f (Tl l1 l2) = Tl (fmap f l1) (fmap f l2)
    fmap f (Tv v1 v2) = Tv (f v1) (f v2)

instance Foldable Tvtl where
    foldr f z (Tl l1 l2) = foldr f (foldr f z l2) l1
    foldr f z (Tv v1 v2) = f v1 (f v2 z)

(Tv x y) +++ (Tv k z) = Tl [x,k] [y,z]
(Tv x y) +++ (Tl l1 l2) = Tl (x : l1) (y : l2)
(Tl l1 l2) +++ (Tv x y)  = Tl (l1 ++ [x]) (l2 ++ [y])
(Tl l1 l2) +++ (Tl l3 l4)  = Tl (l1 ++ l3) (l2 ++ l4)

tvtlconcat list = foldr (+++) (Tl [] []) list
tvtlconcatmap f list = tvtlconcat (fmap f list)

instance Applicative Tvtl where
    pure v = Tl [v] []
    f_tvtl <*> v_tvtl = tvtlconcatmap (\f -> fmap f v_tvtl) f_tvtl

