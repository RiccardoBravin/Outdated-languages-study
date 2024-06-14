-- Consider a Slist data structure for lists that store their length. Define the Slist data structure, and make it
-- an instance of Foldable, Functor, Applicative and Monad.


data Slist sl = Slist {list::[sl], size::Int} deriving (Eq, Show)

makeSlist v = Slist v (length v)


instance Functor Slist where
    fmap f (Slist l s) = Slist (fmap f l) s

instance Foldable Slist where
    foldr f v (Slist l s) = foldr f v l

instance Applicative Slist where
    pure v = Slist [v] 1
    (Slist f sf) <*> (Slist l sl) = Slist (f <*> l) (sf*sl)

instance Monad Slist where
    (Slist ls size) >>= f = makeSlist (ls >>= (\x ->  list(f x)))

    

main :: IO ()
main = do
    let initialList = [1, 2, 3, 4]
    
    -- Test makeSlist function
    let slist = makeSlist initialList
    putStrLn $ "Initial Slist: " ++ show slist
    
    -- Test Functor instance (using fmap)
    let mappedSlist = fmap (* 2) slist
    putStrLn $ "Mapped Slist (Functor): " ++ show mappedSlist
    
    -- Test Foldable instance (using foldr)
    let foldedValue = foldr (+) 0 slist
    putStrLn $ "Folded Value (Foldable): " ++ show foldedValue
    
    -- Test Applicative instance (using <*>)
    let addSlist = pure (+) <*> slist <*> (makeSlist [5, 6, 7, 8])
    putStrLn $ "Added Slists (Applicative): " ++ show addSlist
    
    -- Test Monad instance (using >>=)
    let bindSlist = slist >>= (\x -> makeSlist [x, x * 2])
    putStrLn $ "Bind Slist (Monad): " ++ show bindSlist
