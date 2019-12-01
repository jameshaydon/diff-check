module Main where

import Lib
import Protolude

-- | A function to do stuff.
--
-- REMIND: Whenever 'f' (this function) changes it is important to update 'g'
-- also.
-- REMCHECK 2019-12-01: James Henri Haydon stamped this (vWZ5Z4Tk).
f :: Int -> Int -> Int
f x y = (3 * x) + y

-- start: g

-- | A function to do stuff.
g :: Int -> Int
g x = f x x

-- end: g

main :: IO ()
main = exe
