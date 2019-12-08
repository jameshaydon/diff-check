module Main where

import Lib
import Protolude

-- | A function to do stuff.
--
-- CHECK: 'f' is still compatible with 'g'
-- Whenever 'f' (this function) changes it is important to update 'g' also.
-- This is another description line.
-- STAMP: James Henri Haydon CHECKED 'f' is still compatible with 'g' (AMJA4QEe)
f :: Int -> Int -> Int
f x y =
  (4 * x)
    + (2 * y)
    + (3 * x)
    + (4 * y)
    + (5 * x)
    + (6 * y)
    + (7 * x)

-- start: g

-- | A function to do stuff.
g :: Int -> Int
g x = f x x

-- end: g

main :: IO ()
main = exe
