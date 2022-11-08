{-# OPTIONS_GHC -F -pgmF htfpp #-}
module PicsTest where

import Pics
import Test.Framework

-- Generates a valid pic, with all rows of the same length.
genPic :: Gen Pic
genPic = sized $ \n -> do
    c <- choose(1, n)
    listOf $ vectorOf c $ elements ".#"

selfInverse :: (Pic -> Pic) -> Property
selfInverse f = forAll genPic (\pic -> (f . f) pic == pic)

test_flipH :: IO ()
test_flipH = assertEqual
      [ ".#..."
      , ".#..."
      , ".#..."
      , "#####"
      , ".#..."
      ]
    $ flipH cross

prop_flipH :: Property
prop_flipH = selfInverse flipH

test_flipV :: IO ()
test_flipV = assertEqual
      [ "...#."
      , "#####"
      , "...#."
      , "...#."
      , "...#."
      ]
    $ flipV cross

prop_flipV :: Property
prop_flipV = selfInverse flipV

test_rotate :: IO ()
test_rotate = assertEqual
      [ "...#."
      , "...#."
      , "...#."
      , "#####"
      , "...#."
      ]
    $ rotate cross

prop_rotate :: Property
prop_rotate = selfInverse rotate

test_above :: IO ()
test_above = assertEqual
      [ ".#..."
      , "#####"
      , ".#..."
      , ".#..."
      , ".#..."
      , ".#..."
      , "#####"
      , ".#..."
      , ".#..."
      , ".#..."
      ]
    $ above cross cross

test_beside :: IO ()
test_beside = assertEqual
      [ ".#....#..."
      , "##########"
      , ".#....#..."
      , ".#....#..."
      , ".#....#..."
      ]
    $ beside cross cross

test_invert :: IO ()
test_invert = assertEqual
      [ "#.###"
      , "....."
      , "#.###"
      , "#.###"
      , "#.###"
      ]
    $ invert cross

prop_invert :: Property
prop_invert = selfInverse invert