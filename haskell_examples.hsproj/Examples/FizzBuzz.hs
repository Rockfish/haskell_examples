module FizzBuzz where


fizzBuzz x = 
  let a = if isMod3 x then "Fizz" else ""
      b = a ++ if isMod5 x then "Buzz" else ""
  in if b == "" then show x else b
  where 
    isMod3 x = (mod x 3) == 0
    isMod5 x = (mod x 5) == 0


--fizzBuzz2 x 