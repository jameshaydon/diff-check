module Main where

import Lib
import Protolude

-- start: f

-- | A function to do stuff.
f :: Int -> Int -> Int
f x y = 2 * x + y

-- end: f

-- start: g

-- | A function to do stuff.
g :: Int -> Int
g x = f x x

-- end: g

main :: IO ()
main = exe
