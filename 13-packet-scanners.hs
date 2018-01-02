module Main where

import qualified Data.Text as T
import Control.Monad (forM_)

data Scanner = Scanner { depth :: Int
                       , range :: Int
                       , pos :: Int
                       , dir :: Int }
instance Show Scanner where
  show (Scanner d r p dir) = show d ++ "/" ++ show r ++ "/" ++ show p ++ "/" ++ show dir

strip :: String -> String
strip = T.unpack . T.strip . T.pack

splitOn :: String -> String -> [String]
splitOn sep str = map T.unpack $ T.splitOn (T.pack sep) $ T.pack str

parseScanner :: String -> Scanner
parseScanner s = Scanner d r 0 1
  where [d, r] = map read $ splitOn ": " s

tickOne :: Scanner -> Scanner
tickOne (Scanner depth range pos dir)
  | pos <= 0         = Scanner depth range (pos+1) 1
  | pos >= range - 1 = Scanner depth range (pos-1) (-1)
  | otherwise        = Scanner depth range (pos+dir) dir

tick :: [Scanner] -> [Scanner]
tick = map tickOne

-- traverseFW :: [Scanner] -> Int
traverseFW :: [Scanner] -> [(Int, Int)]
traverseFW = traverseFW' 0
  where
    -- traverseFW' _ [] = 0
    traverseFW' _ [] = []
    traverseFW' layer scanners@((Scanner depth range pos _):rest)
      -- | layer == depth && pos == 0  = (depth*range) + (traverseFW' (layer+1) $ tick rest)
      | layer == depth && pos == 0  = (depth,range) : (traverseFW' (layer+1) $ tick rest)
      | layer == depth && pos /= 0  = traverseFW' (layer+1) $ tick rest
      | otherwise                   = traverseFW' (layer+1) $ tick scanners

severity :: [Scanner] -> Int
severity scanners = sum $ map (uncurry (*)) $ traverseFW scanners

empty :: [a] -> Bool
empty [] = True
empty _ = False
  
findDelay :: [Scanner] -> Int
findDelay scanners = delay
  where
    (delay, _) = head $ filter (empty . traverseFW . snd) $ zip [0..] $ iterate tick scanners
    

main = do
  scanners <- fmap (map parseScanner . lines) getContents

  putStrLn $ "Severity: " ++ (show $ severity scanners)
  putStrLn $ "Delay: " ++ (show $ findDelay scanners)
