module First where
  
import Data.List

sayHello :: String -> IO()
sayHello x = putStrLn ("Hello " ++ x ++ "!")




-----------------

showValue :: Maybe String -> IO()
showValue (Just a) = putStrLn ("Value: " ++ a)
showValue Nothing = putStrLn ("Nothing here")


square :: Either String Int -> Either String Int
square (Left a) = Left ("square: " ++ a)
square (Right b) =  
  if b == 2
  then Left ("can't be 2")
  else Right (b * b)

subone :: Either String Int -> Either String Int
subone (Left a) = Left ("subone: " ++ a)
subone (Right b) =  
  if b < 100
  then Right (b - 1)
  else Left ("too big")
  
times10 :: Either String Int -> Either String Int
times10 (Left a) = Left ("times10: " ++ a)
times10 (Right b) = 
  if b > 20 
  then Right (b * 10)
  else Left ("too small")

process :: Either String Int -> String
process a =
  show $ times10 $ subone $ square a

 
-- 


data Item a = String a | Integer a
  deriving (Show, Eq)

safeShow :: Item a -> Maybe a
safeShow (Integer a) = Just a
safeShow (String _) = Nothing

---










































