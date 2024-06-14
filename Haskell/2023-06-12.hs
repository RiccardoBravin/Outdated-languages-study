import Data.Binary.Get (label)
-- Define a partitioned list data structure, called Part, storing three elements:
-- 1. a pivot value,
-- 2. a list of elements that are all less than or equal to the pivot, and
-- 3. a list of all the other elements.
-- Implement the following utility functions, writing their types:
-- • checkpart, which takes a Part and returns true if it is valid, false otherwise;
-- • part2list, which takes a Part and returns a list of all the elements in it;
-- • list2part, which takes a pivot value and a list, and returns a Part;
-- Make Part an instance of Foldable and Functor, if possible. If not, explain why.


data Part a = Part [a] a [a]

checkpart (Part l p r) = (all (<= p) l) && all (> p) r

part2list (Part l p r) = l ++ [p] ++ r

list2part p [] = Part [] p []
list2part p l = l2p_helper (Part [] p []) l 

l2p_helper x [] = x
l2p_helper (Part l p r) (x:xs) 
 | x <= p = l2p_helper (Part (x:l) p r) xs
 | x > p = l2p_helper (Part l p (x:r)) xs

-- does not necessarily maintain the order 
instance Functor Part where
    fmap f (Part l p r) = Part (fmap f l) (f p) (fmap f r)

instance Foldable Part where
    foldr f z (Part l p r) = foldr f (f p (foldr f z r)) l