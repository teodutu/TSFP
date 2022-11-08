{-# OPTIONS_GHC -F -pgmF htfpp #-}
module BasicsTest where

import Basics
import Test.Framework

reverseTest :: ([Int] -> [Int]) -> IO ()
reverseTest f = do
    assertEqual [] $ f []
    assertEqual [1] $ f [1]
    assertEqual [2, 1] $ f [1, 2]
    assertEqual [3, 2, 1] $ f [1, 2, 3]

reverseProp :: ([Int] -> [Int]) -> [Int] -> Bool
reverseProp f xs = f xs == reverse xs

test_reverseRec1 :: IO ()
test_reverseRec1 = reverseTest reverseRec1

prop_reverseRec1 :: [Int] -> Bool
prop_reverseRec1 = reverseProp reverseRec1

test_reverseRec2 :: IO ()
test_reverseRec2 = reverseTest reverseRec2

prop_reverseRec2 :: [Int] -> Bool
prop_reverseRec2 = reverseProp reverseRec2

test_reverseHO1 :: IO ()
test_reverseHO1 = reverseTest reverseHO1

prop_reverseHO1 :: [Int] -> Bool
prop_reverseHO1 = reverseProp reverseHO1

test_reverseHO2 :: IO ()
test_reverseHO2 = reverseTest reverseHO2

prop_reverseHO2 :: [Int] -> Bool
prop_reverseHO2 = reverseProp reverseHO2

powerSetTest :: ([Int] -> [[Int]]) -> IO ()
powerSetTest f = do
    assertEqual [[]] $ f []
    assertListsEqualAsSets [[], [1]] $ f [1]
    assertListsEqualAsSets
          [[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]]
        $ f [1, 2, 3]

--powerSetProp :: ([Int] -> [[Int]]) -> TestableWithQCArgs
powerSetProp f = withQCArgs (\args -> args { maxSize = 10 }) prop
  where
    prop :: [Int] -> Bool
    prop xs = length (f xs) == 2 ^ length xs

test_powerSetRec :: IO ()
test_powerSetRec = powerSetTest powerSetRec

--prop_powerSetRec :: TestableWithQCArgs
prop_powerSetRec = powerSetProp powerSetRec

test_powerSetHO :: IO ()
test_powerSetHO = powerSetTest powerSetHO 

--prop_powerSetHO :: TestableWithQCArgs
prop_powerSetHO = powerSetProp powerSetHO

test_cartesian2 :: IO ()
test_cartesian2 = assertEqual
      [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (3, 'b')]
    $ cartesian2 ([1, 2, 3] :: [Int]) "ab"

prop_cartesian2 :: [Int] -> [Int] -> Bool
prop_cartesian2 xs ys = length (cartesian2 xs ys) == length xs * length ys

test_cartesian :: IO ()
test_cartesian = assertEqual
    [ [1, 10, 100], [1, 10, 200], [1, 10, 300]
    , [1, 20, 100], [1, 20, 200], [1, 20, 300]
    , [2, 10, 100], [2, 10, 200], [2, 10, 300]
    , [2, 20, 100], [2, 20, 200], [2, 20, 300]
    , [3, 10, 100], [3, 10, 200], [3, 10, 300]
    , [3, 20, 100], [3, 20, 200], [3, 20, 300]
    ]
    (cartesian [[1, 2, 3], [10, 20], [100, 200, 300]] :: [[Int]])

--prop_cartesian :: TestableWithQCArgs
prop_cartesian = withQCArgs (\args -> args { maxSize = 10 }) prop
  where
    prop :: [[Int]] -> Bool
    prop xss = length (cartesian xss) == product (map length xss)