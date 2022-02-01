
module Quicksort_1 where
  
import Data.List

quick1 :: Ord a => [a] -> [a]
quick1 values = 
  if values == [] then values
  else
    let x = head values
        rest = tail values
        smaller = filter (isLess x) rest
        bigger = filter (isGreater x) rest
    in
      (quick1 smaller) ++ [x] ++ (quick1 bigger)
  where
    isLess a b = a > b
    isGreater a b = a <= b


quick2 :: Ord a => [a] -> [a]
quick2 [] = []
quick2 values@_ =     
    let 
      x = head values
      rest = tail values
    in
      (quick2 (filter (x>) rest)) 
      ++ [x] 
      ++ (quick2 (filter (x<=) rest))
    

quick3 :: Ord a => [a] -> [a]
quick3 [] = []
quick3 (x:xs) =     
      (quick3 (filter (x>) xs)) 
      ++ [x] 
      ++ (quick3 (filter (x<=) xs))


quick4 :: Ord a => [a] -> [a]
quick4 [] = []
quick4 (x:xs) = 
  concat [
    quick4 (filter (x>) xs), 
    [x], 
    quick4 (filter (x<=) xs)
   ]


quick5 :: Ord a => [a] -> [a]
quick5 [] = []
quick5 (x:xs) = 
  let parts = partition (<x) xs
  in concat [quick5 (fst parts), [x], quick5 (snd parts)]


-- quick6 :: Ord a => [a] -> [a]
-- quick6 a
--  | a == [] = []
-- quick6 (x:xs) = 
--  let parts = partition (<x) xs
--  in concat [quick6 (fst parts), [x], quick6 (snd parts)]










