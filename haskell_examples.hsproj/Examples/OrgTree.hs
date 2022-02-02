
module OrgTree where
  

data Employee = Employee {
  employeeName :: String,
  employeeReports :: [Employee]
} deriving (Show, Eq)



orgList = [("joe", "sally"),
       ("joe", "fred"),
       ("joe", "george"),
       ("joe", "susan"),
       ("george", "sam"),
       ("george", "alex")]


countEmp :: Employee -> Int
countEmp emp = 1 + sum (map countEmp $ employeeReports emp)

addEmployees :: Employee -> [(String, String)] -> Employee
addEmployees rootEmp listEmps = foldl addEmp rootEmp listEmps

addEmp :: Employee -> (String, String) -> Employee
addEmp emp (mgr, newEmp) = addEmp4 newEmp mgr emp
  
addEmp4 :: String -> String -> Employee -> Employee
addEmp4 empName mgrName emp =
  if mgrName == employeeName emp 
  then Employee mgrName ((Employee empName []):employeeReports emp) 
  else Employee (employeeName emp) (map (addEmp4 empName mgrName) (employeeReports emp))

 
-- Using recusive go pattern 
-- https://kowainik.github.io/posts/haskell-mini-patterns#recursive-go
findEmp :: String -> Employee -> Maybe Employee
findEmp empName rootEmp = foldl go Nothing [rootEmp]
  where 
    go :: Maybe Employee -> Employee -> Maybe Employee
    go (Just emp) _ = Just emp
    go Nothing emp = if empName == employeeName emp then Just emp 
                      else foldl go Nothing $ employeeReports emp

printOrg :: Employee -> IO ()
printOrg emp = go "" emp
  where
    go sp emp = do
      putStrLn (sp ++ (employeeName emp))
      mapM_ (go (sp ++ "  ")) (employeeReports emp)


