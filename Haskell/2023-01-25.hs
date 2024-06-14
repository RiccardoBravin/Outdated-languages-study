import Data.Type.Bool (Not)
-- We want to define a data structure for the tape of a Turing machine: Tape is a parametric data structure with
-- respect to the tape content, and must be made of three components:
-- 1. the portion of the tape that is on the left of the head;
-- 2. the symbol on which the head is positioned;
-- 3. the portion of the tape that is on the right of the head.
-- Also, consider that the machine has a concept of "blank" symbols, so you need to add another component in the
-- data definition to store the symbol used to represent the blank in the parameter type.
-- 1. Define Tape.
-- 2. Make Tape an instance of Show and Eq, considering that two tapes contain the same values if their stored
-- values are the same and in the same order, regardless of the position of their heads.
-- 3. Define the two functions left and right, to move the position of the head on the left and on the right.
-- 4. Make Tape an instance of Functor and Applicative.

data Tape a = Tape [a] (Maybe a) [a]

instance Show a => Show (Tape a) where
    show (Tape l (Just v) r) = show (reverse l) ++ ">" ++ show v ++ "<" ++ show r
    show (Tape l Nothing r) = show (reverse l) ++ "><" ++ show r

instance Eq a => Eq (Tape a) where
    (Tape l (Just v) r) == (Tape l' (Just v') r') = ((reverse l) ++ [v] ++ r) == ((reverse l') ++ [v'] ++ r')
    (Tape l (Just v) r) == (Tape l' Nothing r') = ((reverse l) ++ [v] ++ r) == ((reverse l') ++ r')
    (Tape l Nothing r) == (Tape l' (Just v') r') = ((reverse l) ++ r) == ((reverse l') ++ [v'] ++ r')
    (Tape l Nothing r) == (Tape l' Nothing r') = ((reverse l) ++ r) == ((reverse l') ++ r')

left (Tape (l:ls) (Just v) r) = Tape ls (Just l) (v : r)
left (Tape (l:ls) Nothing r) = Tape ls (Just l) r
left (Tape [] (Just v) r) = Tape [] Nothing (v : r)
left (Tape [] Nothing r) = Tape [] Nothing r



right (Tape l (Just v) (r:rs)) = Tape (v : l) (Just r) rs
right (Tape l Nothing (r:rs)) = Tape l (Just r) rs
right (Tape l (Just v) []) = Tape (v : l) Nothing []
right (Tape l Nothing []) = Tape l Nothing []


instance Functor Tape where
    fmap f (Tape l (Just v) r) = Tape (fmap f l) (Just (f v)) (fmap f r)
    fmap f (Tape l Nothing r) = Tape (fmap f l) Nothing (fmap f r)

instance Applicative Tape where
    pure x = Tape [] (Just x) []
    --to each element in the second tape we apply the corresponding function from the first tape
    (Tape fx (Just fc) fy) <*> (Tape x (Just c) y) = Tape (zipApp fx x) (Just (fc c)) (zipApp fy y)
        where zipApp x y = [f x | (f,x) <- zip x y]
    
    (Tape fx _ fy) <*> (Tape x Nothing y) = Tape (zipApp fx x) Nothing (zipApp fy y)
        where zipApp l1 l2 = [f x | (f,x) <- zip l1 l2]