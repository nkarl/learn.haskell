{-Haskell is available for Windows, Mac, and Linux. Here's the download page: http://www.haskell.org/platform/.

We will be using the HUnit unit testing package in CptS 355. -}

{- Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "runTestTT tests" at the Haskell prompt.  -}

-- CPTS-355
-- Name: Charles Norden


module HW1SampleTests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1


p1a_test1 = TestCase (assertEqual "getUniqueRight-test1" "defcba"  (getUniqueRight "abcaadbbefcba") ) 
p1a_test2 = TestCase (assertEqual "getUniqueRight-test2" [('a',1),('c',1),('b',1),('a',2)]  (getUniqueRight [('a',1),('a',2),('b',1),('a',2),('c',1),('b',1),('a',2)]) ) 
p1a_test3 = TestCase (assertEqual "getUniqueRight-test3" ["Let","snow","rain","let","it","hail"]  (getUniqueRight ["Let","it","snow", "let","it", "rain", "let", "it","hail"]) ) 

p1b_test1 = TestCase (assertEqual "getUniqueLeft-test1" "abcdef"  (getUniqueLeft "abcaadbbefcba") ) 
p1b_test2 = TestCase (assertEqual "getUniqueLeft-test2" [('a',1),('a',2),('b',1),('c',1)]  (getUniqueLeft [('a',1),('a',2),('b',1),('a',2),('c',1),('b',1),('a',2)]) ) 
p1b_test3 = TestCase (assertEqual "getUniqueLeft-test3" ["Let","it","snow","let","rain","hail"]  (getUniqueLeft ["Let","it","snow", "let","it", "rain", "let", "it","hail"]) ) 

myCatsLog = [((7,2020),[("Oceanfish",7),("Tuna",1),("Whitefish",3),("Chicken",4),("Beef",2)]),
             ((8,2020),[("Oceanfish",6),("Tuna",2),("Whitefish",1),("Salmon",3),("Chicken",6)]),
             ((9,2020),[("Tuna",3),("Whitefish",3),("Salmon",2),("Chicken",5),("Beef",2),("Turkey",1),("Sardines",1)]),
             ((10,2020),[("Whitefish",5),("Sardines",3),("Chicken",7),("Beef",3)]),
             ((11,2020),[("Oceanfish",3),("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",1)]),
             ((12,2020),[("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",4),("Sardines",1)]),              
             ((1,2021),[("Chicken",7),("Beef",3),("Turkey",4),("Whitefish",1),("Sardines",2)])
 ]

p2a_test1 = TestCase (assertEqual "(cansInLog-test1)" 17 (cansInLog [("Oceanfish",7),("Tuna",1),("Whitefish",3),("Chicken",4),("Beef",2)]) ) 
p2a_test2 = TestCase (assertEqual "(cansInLog-test2)" 19 (cansInLog  [("Oceanfish",3),("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",1),("Sardines",3)]) ) 

p2b_test1 = TestCase (assertEqual "(numCans-test1)" 103 (numCans myCatsLog 2020) ) 
p2b_test2 = TestCase (assertEqual "(numCans-test2)" 17 (numCans myCatsLog 2021) ) 

p2c_test1 = TestCase (assertEqual "(getMonths-test1)" [(7,2020),(8,2020)] (getMonths myCatsLog 4 "Oceanfish") ) 
p2c_test2 = TestCase (assertEqual "(getMonths-test2)" [(8,2020),(10,2020),(1,2021)] (getMonths myCatsLog 5 "Chicken" )) 
p2c_test3 = TestCase (assertEqual "(getMonths-test3)" [(12,2020),(1,2021)] (getMonths myCatsLog 1 "Turkey") )
p2c_test4 = TestCase (assertEqual "(getMonths-test4)" [(8,2020),(9,2020),(11,2020),(12,2020)] (getMonths myCatsLog 1 "Tuna") )
p2c_test5 = TestCase (assertEqual "(getMonths-test5)" [(7,2020),(9,2020),(10,2020)] (getMonths myCatsLog 2 "Whitefish") )
p2c_test6 = TestCase (assertEqual "(getMonths-test6)" [] (getMonths myCatsLog 9 "Beef") ) 

p3_test1 = TestCase (assertEqual "deepCount-test1" 5  (deepCount 5 [[1,2,3],[5,5],[4,5,6],[7,1,2,3,4,5],[5,6]]) ) 
p3_test2 = TestCase (assertEqual "deepCount-test2" 3  (deepCount "a" [["a","b","c"],["b","c"],["b","e","g"],["a","h","c","d"],["d"],[],["h","a"]]) ) 
p3_test3 = TestCase (assertEqual "deepCount-test3" 0  (deepCount 10 [[1,2,3],[1,2],[4,5,6],[7,1,2,3,4,5],[1],[], [5,6]]) ) 
p3_test4 = TestCase (assertEqual "deepCount-test4" 0  (deepCount 1 []) ) 

p4_test1 = TestCase (assertEqual "clusterConsequtive-test1" [[1,2,3],[5,6,7,8,9],[2,3],[11,12]]  (clusterConsecutive [1,2,3,5,6,7,8,9,2,3,11,12]) ) 
p4_test2 = TestCase (assertEqual "clusterConsequtive-test2" [[1],[3],[5],[7,8],[10],[13]]  (clusterConsecutive [1,3,5,7,8,10,13]) ) 
p4_test3 = TestCase (assertEqual "clusterConsequtive-test3" [[1]]  (clusterConsecutive [1]) ) 
p4_test4 = TestCase (assertEqual "clusterConsequtive-test4" [[2],[1]]  (clusterConsecutive [2,1]) ) 
p4_test5 = TestCase (assertEqual "clusterConsequtive-test"  [] (clusterConsecutive []) ) 

tests = TestList [ TestLabel "Problem 1a- test1 " p1a_test1,
                   TestLabel "Problem 1a- test2 " p1a_test2,  
                   TestLabel "Problem 1a- test3 " p1a_test3,  
                   TestLabel "Problem 1b- test1 " p1b_test1,
                   TestLabel "Problem 1b- test2 " p1b_test2,  
                   TestLabel "Problem 1b- test3 " p1b_test3,  
                   TestLabel "Problem 2a- test1 " p2a_test1, 
                   TestLabel "Problem 2a- test2 " p2a_test2, 
                   TestLabel "Problem 2b- test1 " p2b_test1, 
                   TestLabel "Problem 2b- test2 " p2b_test2,
                   TestLabel "Problem 2c- test1 " p2c_test1, 
                   TestLabel "Problem 2c- test2 " p2c_test2, 
                   TestLabel "Problem 2c- test3 " p2c_test3, 
                   TestLabel "Problem 2c- test4 " p2c_test4, 
                   TestLabel "Problem 2c- test5 " p2c_test5, 
                   TestLabel "Problem 2c- test6 " p2c_test6, 
                   TestLabel "Problem 3- test1 " p3_test1, 
                   TestLabel "Problem 3- test2 " p3_test2, 
                   TestLabel "Problem 3- test3 " p3_test3,
                   TestLabel "Problem 3- test4 " p3_test4, 
                   TestLabel "Problem 4- test1 " p4_test1, 
                   TestLabel "Problem 4- test2 " p4_test2,
                   TestLabel "Problem 4- test3 " p4_test3, 
                   TestLabel "Problem 4- test4 " p4_test4, 
                   TestLabel "Problem 4- test5 " p4_test5 
                 ] 
                  
-- shortcut to run the tests
run = runTestTT  tests
