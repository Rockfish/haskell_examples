{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where -- simple example

example = 1

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

c :: a -> b -> a 
c x y = x

c' :: a -> b -> b 
c' x y = y

r :: [a] -> [a] 
--r (x:xs) = xs
r x = tail x



co :: (b -> c) -> (a -> b) -> a -> c 
co bToc aTob a = bToc $ aTob a

i :: Int
i = 1



mTh = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of 
  True -> (\x -> x + 1) n
  False -> n
  --where f n = n + 1

addFive x y = (if x > y then y else x) + 5

addFiveL = (\x -> \y -> (if x > y then y else x) + 5)

mflipL f = \x -> \y -> f y x

mflip f x y = f y x

----- Chapter 7 - pattern matching

newtype Username =
  Username String
  
newtype AccountNumber =
  AccountNumber Integer
  
data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO () 

printUser UnregisteredUser =
  putStrLn "UnregisteredUser"

printUser (RegisteredUser 
  (Username name)
  (AccountNumber acctNum)) 
      = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica 
  deriving (Eq, Ord, Show)

data Penguin = Penguin WherePenguinsLive 
  deriving (Eq, Show)

humboldt = Penguin SouthAmerica
gentoo = Penguin Antarctica
macaroni = Penguin Antarctica
little = Penguin Australia
galapagos = Penguin Galapagos

penguinType :: Penguin -> String
penguinType (Penguin SouthAmerica) = "humboldt"
penguinType (Penguin Antarctica) = "gentoo"
penguinType (Penguin Australia) = "macaroni"
penguinType (Penguin Galapagos) = "galapagos"
penguinType (Penguin SouthAfrica) = "zulu"


-- Pattern matching and case statements

k :: (a, b) -> a
k (a, b) = a

ff :: (a,b,c) -> (d,e,f) -> ((a, d), (c,f))
ff (a,b,c) (d,e,f) = ((a, d), (c,f))

penguinType2 pt =
  case pt of
    Penguin SouthAmerica -> "humboldt"
    Penguin Antarctica -> "gentoo"
    Penguin Australia -> "macaroni"
    Penguin Galapagos -> "galapagos"
    Penguin SouthAfrica -> "zulu"
    
funcC x y =
  case x > y of
    True -> x
    False -> y

-- guards

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x 
  | y >= 0.9 = 'A' 
  | y >= 0.8 = 'B' 
  | y >= 0.7 = 'C' 
  | y >= 0.59 = 'D' 
  | otherwise = 'F' 
  where y = x / 100

-- composion

f :: Show a => a -> String
f a = show a

g :: String -> Int
g b = read b

h = g . f

ri :: String -> Int -> Int
ri a b = b + (read a)

isNum :: Char -> Bool
isNum a
  | a == '0' = True
  | a == '1' = True
  | a == '2' = True
  | a == '3' = True
  | a == '4' = True
  | a == '5' = True
  | a == '6' = True
  | a == '7' = True
  | a == '8' = True
  | a == '9' = True
  | otherwise = False
  
tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10 

mod10 a = mod a 10
div10 a = div a 10
tensD = mod10 . div10

tensD2 :: Integral a => a -> a
tensD2 a = mod (div a 10) 10


foldBool3 :: a -> a -> Bool -> a
foldBool3 a b c 
  | c == True = b
  | c == False = a


roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show






