module Main where

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
  where input' = concat $ replicate 64 input
    
dense :: [Int] -> [Int]
dense = unfoldr dense'
  where
    dense' [] = Nothing
    dense' xs = Just (foldl1 xor $ take 16 xs, drop 16 xs)

hexify :: [Int] -> String
hexify = concatMap (printf "%02x")

strip :: String -> String
strip = T.unpack . T.strip . T.pack

parseInput :: String -> [Int]
parseInput = map (read . T.unpack) . T.splitOn (T.singleton ',') . T.pack

main = do
  input <- fmap strip getContents
  let simpleInput = parseInput input
      asciiInput = map ord input ++ [17, 31, 73, 47, 23]
      (a:b:_) = simpleKnotHash 256 simpleInput
  print $ (a*b)
  putStrLn $ fullKnotHash 256 asciiInput & dense & hexify
