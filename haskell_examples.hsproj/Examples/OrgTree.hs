
module OrgTree where
  

data Employee = Employee {
  employeeName :: String,
  employeeReports :: [Employee]
} deriving (Show, Eq)



org = [("joe", "sally"),
       ("joe", "fred"),
       ("joe", "george"),
       ("joe", "susan"),
       ("george", "sam"),
       ("george", "alex")]


countEmp :: Employee -> Int
countEmp emp = 1 + sum (map countEmp $ employeeReports emp)

newOrg :: Employee -> [(String, String)] -> Employee
newOrg ceo empList = foldl addEmp ceo empList

addEmp :: Employee -> (String, String) -> Employee
addEmp emp (mgr, newEmp) = addEmp4 newEmp mgr emp

  
addEmp4 :: String -> String -> Employee -> Employee
addEmp4 empName mgrName emp =
  if mgrName == employeeName emp then Employee mgrName ((Employee empName []):employeeReports emp) 
  else Employee (employeeName emp) (map (addEmp4 empName mgrName) (employeeReports emp))

 
printOrg :: String -> Employee -> IO ()
printOrg sp emp = do
  putStrLn (sp ++ (employeeName emp))
  mapM_ (printOrg (sp ++ "  ")) (employeeReports emp)



findEmp :: String -> Employee -> Maybe Employee
findEmp n rootEmp = foldl (_findEmp n) Nothing [Just rootEmp]

_findEmp :: String -> Maybe Employee -> Maybe Employee -> Maybe Employee
_findEmp _ Nothing Nothing = Nothing
_findEmp _ (Just emp) _ = Just emp
_findEmp n _ (Just emp) = 
  if n == employeeName emp then Just emp 
  else foldl (_findEmp n) Nothing $ map Just $ employeeReports emp
  

-- Using recusive go pattern 
-- https://kowainik.github.io/posts/haskell-mini-patterns#recursive-go
findEmpGo :: String -> Employee -> Maybe Employee
findEmpGo empName rootEmp = foldl go Nothing [Just rootEmp]
  where 
    go :: Maybe Employee -> Maybe Employee -> Maybe Employee
    go Nothing Nothing = Nothing
    go Nothing (Just emp) = if empName == employeeName emp then Just emp 
                            else foldl go Nothing $ map Just $ employeeReports emp
    go ans _ = ans

findEmpGo2 :: String -> Employee -> Maybe Employee
findEmpGo2 empName rootEmp = foldl go Nothing [rootEmp]
  where 
    go :: Maybe Employee -> Employee -> Maybe Employee
    go Nothing emp = if empName == employeeName emp then Just emp 
                      else foldl go Nothing $ employeeReports emp
    go ans _ = ans
