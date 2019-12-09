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
    + (1 * y)
    + (2 * x)
    + (2 * y)
    + (4 * x)
    + (5 * y)
    + (6 * x)

-- | A function to do stuff.
-- CHECK: should check this.
g :: Int -> Int
g x =
  f x x
    + f (f x x) x

main :: IO ()
main = exe
