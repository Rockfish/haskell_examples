
module Scratch where
  
--
--join x xs = map make xs
--  where make y = (x, y)


j2 xs x = map (pair x) xs
  where pair x' y' = (x', y')
  

combine :: [a] -> [b] -> [(a, b)]
combine xnums ynums = concat (map (join xnums) ynums)
  where join xs y = map (pair y) xs
        pair y' x' = (x', y')
  