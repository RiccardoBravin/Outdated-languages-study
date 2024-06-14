import Distribution.Compat.Prelude (readMaybe)
import Data.Time (getCurrentTime, toGregorian, UTCTime (utctDay))

--more monads
data Result a = Ok a | Err deriving (Eq, Show)

data Expr = Val Int | Div Expr Expr deriving (Eq, Show)

x = Div (Div (Val 1) (Val 0)) (Val 4)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = div (eval x) (eval y)

safediv :: Int -> Int -> Result Int
safediv n m = if m == 0 then Err else Ok (div n m)

eval' :: Expr -> Result Int
eval' (Val n) = Ok n
eval' (Div x y) = case eval' x of
    Err -> Err
    Ok n -> case eval' y of
        Err-> Err
        Ok m -> safediv n m
--questa funzione ha una scrivibilità pessima, per questo possiamo usare una monade
--la monade è una struttura che permette di scrivere in modo più semplice le funzioni che lavorano su un tipo di dato

--simuliamo una sorta di monade per sistemare eval'
bind :: Result Int -> (Int -> Result Int) -> Result Int
m `bind` f = case m of
        Err-> Err
        Ok x -> f x
-- bind è una funzione che prende un risultato e una funzione che prende un intero e restituisce un risultato e a sua volta restituisce un risultato

eval'' :: Expr -> Result Int
eval'' (Val n) = Ok n
eval'' (Div x y) = eval'' x `bind` (\n -> eval'' y `bind` safediv n)
--this still is not very readable, so we can use the do notation if we define the monad for Result


--to define the monad for result we must first make it a functor and applicative functor
instance Functor Result where
    fmap f (Ok x) = Ok (f x)
    fmap _ Err = Err

instance Applicative Result where
    pure = Ok
    Ok f <*> Ok x = Ok $ f x
    _ <*> Err = Err
    Err <*> _ = Err

instance Monad Result where
    Ok x  >>= f = f x
    Err >>= _ = Err
    --return = pure --default implementation 

--rules of moads
-- Left identity: return a >>= f = f a
-- Right identity: m >>= return = m
-- Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)

--example: (Ok 3 >>= (\x -> Ok (x+1))) >>= (\x -> Ok (x*2)) = Ok 8
--         Ok 3 >>= (\a -> (\x -> Ok (x+1)) a >>= (\x -> Ok (x*2)))  = Ok 8 

--now we can define the monad version of eval
mEval :: Expr -> Result Int
mEval (Val x) = Ok x
mEval (Div x y) = do
    m <- mEval x
    n <- mEval y
    safediv m n 

--How do we write a working program?

greet name = "Hello " ++ name ++ "!"

--main :: IO () --IO is a monad which we use to communicate with the outside world
--() means that we dont care what the type of the result is
main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn $ greet name
    --an equivalent way to write this is:
    --main = putStrLn "Your name?" >> getLine >>= (\name -> putStrLn $ greet name)
    putStrLn "What's your age?"
    msg <- getLine
    let age = readMaybe msg :: Maybe Int
    currentTime <- getCurrentTime
    let (year, _, _) = toGregorian $ utctDay currentTime
    case age of
        Just a -> putStrLn $ "You were born in " ++ show (fromInteger year - a)
        _ -> putStrLn "I didn't understand your age"



