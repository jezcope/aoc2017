module Main where

import qualified Data.Text as T
import Data.Maybe (mapMaybe)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

splitOn :: String -> String -> [String]
splitOn sep = map T.unpack . T.splitOn (T.pack sep) . T.pack

parseScanner :: String -> (Int, Int)
parseScanner s = (d, r)
  where [d, r] = map read $ splitOn ": " s

traverseFW :: Int -> [(Int, Int)] -> [Int]
traverseFW delay = mapMaybe caught
  where
    caught (d, r) = if (d + delay) `mod` (2*(r-1)) == 0
      then Just (d * r)
      else Nothing

severity :: [(Int, Int)] -> Int
severity = sum . traverseFW 0

findDelay :: [(Int, Int)] -> Int
findDelay scanners = head $ filter (null . flip traverseFW scanners) [0..]

main = do
  scanners <- fmap (map parseScanner . lines) getContents

  putStrLn $ "Severity: " ++ (show $ severity scanners)
  putStrLn $ "Delay: " ++ (show $ findDelay scanners)
