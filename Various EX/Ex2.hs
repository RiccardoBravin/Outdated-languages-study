
--Path towards Monads

--definition of tree for commodity
data Tree a = TNothing | Leaf a | Branch (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch l r) = "(" ++  show l ++ " " ++  show r ++ ")"

--The concept of maybe
data MyMaybe a = MyNothing | MyJust a

--foldable as an instance 
instance Foldable MyMaybe where
    foldr _ z MyNothing = z --implementation done with foldr beacuse it is lazy and should perform better (even better to overwrite with foldl')
    foldr f z (MyJust x) = f x z

instance Foldable Tree where
    foldr _ z TNothing = z
    foldr f z (Leaf x) = f x z
    foldr f z (Branch l r) = foldr f (foldr f z r) l

--functor is the class of types that offers a map operation called fmap
instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust a) = MyJust (f a)

instance Functor Tree where --apply to all elements of the structure the function 
    fmap _ TNothing = TNothing
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

--applicative functors
instance Applicative MyMaybe where
    pure = MyJust
    MyJust f <*> m = fmap f m --apply the function f extracted from the MyJust to the value inside the structure m 
    MyNothing <*> _ = MyNothing

--Applicative functor rules
--pure id <*> v = v --identity
--pure f <*> pure x = pure (f x) --homomorphism
--u <*> pure y = pure ($ y) <*> u --interchange
--pure (.) <*> u <*> v <*> w = u <*> (v <*> w) --composition


--for a tree to be able to define the applicative functor we must define some other functions
--tree concatenation
tconc TNothing t = t
tconc t TNothing = t
tconc t1 t2 = Branch t1 t2 --concatenation of two trees
tconcat t = foldr tconc TNothing t  --concatenation of a list of trees
tconcatmap f t = tconcat $ fmap f t

instance Applicative Tree where
    pure a = Leaf a
    fs <*> xs = tconcatmap (\f -> fmap f xs) fs --apply the function f extracted from the tree fs to the tree xs generating a new tree in which there are 
                                                --subtrees with the function applied to the values of the original tree

--big tree to test the foldable instance
treeInst = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4))
ftreeInst = Branch (Branch (Leaf (+ 1)) (Leaf (+ (-1)))) (Branch (Leaf (* 2)) (Leaf (* 10)))

--monads
class Applicative m => MyMonad m where
-- Sequentially compose two actions, passing any value produced
-- by the first as an argument to the second.
    (>>=) :: m a -> (a -> m b) -> m b
-- Sequentially compose two actions, discarding any value produced
-- by the first, like sequencing operators (such as the semicolon)
-- in imperative languages.
    (>>) :: m a -> m b -> m b
    m >> k = m Main.>>= \_ -> k
-- Inject a value into the monadic type.
    return :: a -> m a
    return = pure
-- Fail with a message.
    fail :: String -> m a
    fail s = error s


instance MyMonad MyMaybe where
    return a = MyJust a
    MyNothing >>= _ = MyNothing
    (MyJust x) >>= f  = f x
    fail _ = MyNothing

instance MyMonad Tree where
    TNothing >>= _ = TNothing
    xs >>= f = tconcatmap f xs
    fail _ = TNothing


main :: IO ()
main = do
    print $ treeInst
    print $ foldr (+) 0 treeInst
    print $ sum treeInst --having defined the foldable instance, we can use the sum function
    print $ fmap (+1) treeInst
    print $ ftreeInst <*> treeInst
    print $ treeInst Main.>>= (\x -> Main.return (x+1))


