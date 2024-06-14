import Data.Foldable (Foldable(toList))
-- A deque, short for double-ended queue, is a list-like data structure that supports efficient element
-- insertion and removal from both its head and its tail. Recall that Haskell lists, however, only support O(1)
-- insertion and removal from their head.
-- Implement a deque data type in Haskell by using two lists: the first one containing elements from the
-- initial part of the list, and the second one containing elements form the final part of the list, reversed.
-- In this way, elements can be inserted/removed from the first list when pushing to/popping the deque's
-- head, and from the second list when pushing to/popping the deque's tail.
-- 1) Write a data type declaration for Deque.
-- 2) Implement the following functions:
-- • toList: takes a Deque and converts it to a list
-- • fromList: takes a list and converts it to a Deque
-- • pushFront: pushes a new element to a Deque's head
-- • popFront: pops the first element of a Deque, returning a tuple with the popped element and the
-- new Deque
-- • pushBack: pushes a new element to the end of a Deque
-- • popBack: pops the last element of a Deque, returning a tuple with the popped element and the new
-- Deque
-- 3) Make Deque an instance of Eq and Show.
-- 4) Make Deque an instance of Functor, Foldable, Applicative and Monad.
-- You may rely on instances of the above classes for plain lists.

data Deque a = Deque [a] [a]

toList (Deque t b) = t ++ reverse b
fromList l = let half = div (length l) 2 in
                Deque (take half l) (take half (reverse l))
pushFront x (Deque t b) = Deque (x : t) b

popFront (Deque (t : ts) b) = (t, Deque ts b)
popFront (Deque [] []) = error "Cannot pop from empty queue"
popFront (Deque [] [b]) = (b, Deque [] [])
popFront (Deque [] b) = popFront (fromList b) -- since b is just a list make it a new deque and pop from that using the other matches

pushBack x (Deque t b) = Deque t (x:b)

popBack (Deque t (b : bs)) = (b, Deque t bs)
popBack (Deque [] []) = error "Cannot pop from empty queue"
popBack (Deque [t] []) = (t , Deque [] [])
popBack (Deque t []) = popBack (fromList t)

instance Show a => Show (Deque a) where
    show (Deque t b) = show t ++ show (reverse b)

instance Eq a => Eq (Deque a) where
    d1 == d2 = Main.toList d1 == Main.toList d2


instance Functor Deque where
    fmap f (Deque t b) = Deque (map f t) (map f b)

instance Foldable Deque where
    foldr f z dk = foldr f z (Main.toList dk)

instance Applicative Deque where
    pure x = Deque [x] []
    fdk <*> dk = fromList (Main.toList fdk <*> Main.toList dk)


instance Monad Deque where
     d >>= f = fromList (concatMap (Main.toList . f) (Main.toList d) )