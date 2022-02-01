module Arith3Broken where
  
main :: IO () 

main = do
    print (1 + 2)
    putStrLn (show 10)
    print (show (negate (-1))) 
    print ((+) 0 blah) 
  where blah = negate 1
  
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x


data A 
data B 
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

data X 
data Y 
data Z

xz :: X -> Z 
xz = undefined

yz :: Y -> Z 
yz = undefined

xform :: (X, Y) -> (Z, Z) 
xform (x, y) = (xz x, yz y)


munge :: (x -> y)
          -> (y -> (w, z))
          -> x
          -> w 
  
munge xToy yTowz x = fst (yTowz (xToy x))


-- type classes

data Trival = Trival'

instance Eq Trival where
  Trival' == Trival' = True

-----------

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where 
  (==) Mon Mon = True 
  (==) Tue Tue = True 
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') = 
          weekday == weekday'
          && dayOfMonth == dayOfMonth'

------

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'



---------------

data TisAnInteger a =
       TisAn Integer

instance Eq (TisAnInteger a) where
  (==) (TisAn a) (TisAn a') = a == a'
  

data TwoIntegers a b 
  = Two Integer Integer

instance Eq (TwoIntegers a b) where
  (==) (Two a b) (Two a' b') = a == a' && b == b'
  

data StringOrInt a = 
  TisAnInt Int | TisAString String

instance Eq (StringOrInt a) where
  (==) (TisAString a) (TisAString a') = a == a'
  (==) (TisAnInt a) (TisAnInt a') = a == a'


data MyPair a b = MyPair a b

instance (Eq a, Eq b) => Eq (MyPair a b) where
  (==) (MyPair a b) (MyPair a' b') = a == a' && b == b'


data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye a) (Goodbye a') = a == a'
  -- (==) (Hello a) (Goodbye b) = a == b 
  -- a and b can be diffent types so it doesn't work

--------

class FooClass a where
  (%%) :: a -> a -> Bool
  -- fromNumber :: Integer -> a 
  -- toNumber :: a -> Integer

data FooData a = FooData a

instance (Eq a) => FooClass (FooData a) where 
  (%%) (FooData a) (FooData a') = a == a'
  
instance (Show a) => Show (FooData a) where
  show (FooData a) = "FooData: " ++ (show a)

------------

class Numberish a where 
  fromNumber :: Integer -> a 
  toNumber :: a -> Integer
  
newtype Age =
  Age Integer 
  deriving (Eq, Show)

newtype Year =
  Year Integer 
  deriving (Eq, Show)

instance Numberish Age where 
  fromNumber n = Age n 
  toNumber (Age n) = n
    
instance Numberish Year where 
  fromNumber n = Year n 
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a 
sumNumberish a a' = fromNumber summed
  where 
    intA = toNumber a 
    intA' = toNumber a' 
    summed = intA + intA'

addNumberish :: Age -> Year -> Integer
addNumberish a b = inta + intb
  where
    inta = toNumber a
    intb = toNumber b


idOne :: Int
idOne = 1

data Box a = Something a | Empty

----

data Person = Person Bool 
--  deriving(Show)

instance Show Person where
  show (Person a) = show a

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

----

data Mood = Blah
            | Woot 
   deriving (Show, Eq)
   

settleDown x = 
  if x == Woot then Blah
  else x 

----

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object 
  deriving (Eq, Show)
  
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

----

data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

--phew = Papu "chases" True
truth = Papu (Rocks "chomskydoz") (Yeah True)
consequence = Papu (Rocks "apple") (Yeah False)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

instance Ord (Papu) where
  (>) (Papu a b) (Papu a' b') = a > a' && b > b'
  (<=) (Papu a b) (Papu a' b') = a <= a' && b <= b' 
  
comparePapus :: Papu -> Papu -> Bool 
comparePapus p p' = p > p'

-- Count bits in binary representation

bitCount :: Integral a => a -> a
bitCount 0 = 0
bitCount a = let (q, r) = quotRem a 2 in r + bitCount q
  
  













