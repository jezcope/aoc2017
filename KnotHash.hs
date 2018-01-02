module KnotHash where

import Data.Char (ord)
import Data.Bits (xor)
import Data.Function ((&))
import Data.List (unfoldr)
import Text.Printf (printf)
import qualified Data.Text as T

rotate :: Int -> [Int] -> [Int]
rotate 0 xs = xs
rotate n xs = drop n' xs ++ take n' xs
  where n' = n `mod` length xs

simpleKnotHash :: Int -> [Int] -> [Int]
simpleKnotHash size input = foldl step [0..size-1] input' & rotate (negate finalPos)
  where
    input' = zip input [0..]
    finalPos = sum $ zipWith (+) input [0..]
    reversePart xs n  = (reverse $ take n xs) ++ drop n xs
    step xs (n, skip) = reversePart xs n & rotate (n+skip)

fullKnotHash :: Int -> [Int] -> [Int]
fullKnotHash size input = simpleKnotHash size input'
  where input' = concat $ replicate 64 (input ++ [17, 31, 73, 47, 23])
    
dense :: [Int] -> [Int]
dense = unfoldr dense'
  where
    dense' [] = Nothing
    dense' xs = Just (foldl1 xor $ take 16 xs, drop 16 xs)

hexify :: [Int] -> String
hexify = concatMap (printf "%02x")

hexHash :: [Int] -> String
hexHash = hexify . dense . fullKnotHash 256
