
module Students where
  
data Student = Student {
   name :: String,
   grade :: Int
} deriving (Eq, Show)


initializeStudents :: Int -> [Student]
initializeStudents count = map (\_ -> Student "" 0) [1..count]


initStudents :: [String] -> [Student]
initStudents names = map (\name -> Student name 0) names

sumGrades :: [Student] -> Int
sumGrades students = sum $ map grade students

-- type signature of map is (a -> b) -> [a] -> [b]

sumGrades2 :: [Student] -> Int
sumGrades2 students = sum (map grade students)



averageGrades :: [Student] -> Double
averageGrades students = fromIntegral (sumGrades students) / fromIntegral (length students)

