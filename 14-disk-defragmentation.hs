module Main where

import Prelude hiding (length, filter, take)
import Data.Char (ord)
import Data.Sequence
import Data.Foldable hiding (length)
import Data.Ix (inRange)
import Data.Function ((&))
import Data.Maybe (fromJust, mapMaybe, isJust)
import qualified Data.Set as Set
import Text.Printf (printf)
import System.Environment (getArgs)

import KnotHash

data Sector = Free | Used | Mark Int
  deriving (Eq)

type GridRow = Seq Sector
type Grid = Seq (GridRow)

instance Show Sector where
  show Free = "   ."
  show Used = "   #"
  show (Mark i) = printf "%4d" i

subGrid :: Int -> Grid -> Grid
subGrid n = fmap (take n) . take n

printRow :: GridRow -> IO ()
printRow row = do
  mapM_ (putStr . show) row
  putStr "\n"

printGrid :: Grid -> IO ()
printGrid = mapM_ printRow

makeKey :: String -> Int -> String
makeKey input n = input ++ "-" ++ show n

stringToGridRow :: String -> GridRow
stringToGridRow = fromList . map convert
  where convert x
          | x == '1' = Used
          | x == '0' = Free

makeRow :: String -> Int -> GridRow
makeRow input n = stringToGridRow $ concatMap (printf "%08b")
  $ dense $ fullKnotHash 256
  $ map ord $ makeKey input n

makeGrid :: String -> Grid
makeGrid input = fromList $ map (makeRow input) [0..127]

countEqual :: Sector -> Grid -> Int
countEqual x = sum . fmap (length . filter (==x))

countUsed = countEqual Used
countFree = countEqual Free

findUnmarked :: Grid -> Maybe (Int, Int)
findUnmarked g
  | y == Nothing = Nothing
  | otherwise = Just (fromJust x, fromJust y)
  where
    hasUnmarked row = isJust $ elemIndexL Used row
    x = findIndexL hasUnmarked g
    y = case x of
      Nothing -> Nothing
      Just x' -> elemIndexL Used $ index g x'

floodFill :: Sector -> Sector -> (Int, Int) -> Grid -> Grid
floodFill t r (x, y) g
  | inRange (0, length g - 1) x
    && inRange (0, length g - 1) y
    && elem == t =
      let newRow = update y r row
          newGrid = update x newRow g
      in newGrid
         & floodFill t r (x+1, y)
         & floodFill t r (x-1, y)
         & floodFill t r (x, y+1)
         & floodFill t r (x, y-1)
  | otherwise = g
  where
    row = g `index` x
    elem = row `index` y

markNextGroup :: Int -> Grid -> Maybe Grid
markNextGroup i g = case findUnmarked g of
                      Nothing -> Nothing
                      Just loc -> Just $ floodFill Used (Mark i) loc g

markAllGroups :: Grid -> Grid
markAllGroups g = markAllGroups' 1 g
  where
    markAllGroups' i g = case markNextGroup i g of
      Nothing -> g
      Just g' -> markAllGroups' (i+1) g'

onlyMarks :: GridRow -> [Int]
onlyMarks = mapMaybe getMark . toList
  where
    getMark Free = Nothing
    getMark Used = Nothing
    getMark (Mark i) = Just i

countGroups :: Grid -> Int
countGroups g = Set.size groupSet
  where
    groupSet = foldl' Set.union Set.empty $ fmap rowToSet g
    rowToSet = Set.fromList . toList . onlyMarks
  
main = do
  input <- fmap head getArgs
  let grid = makeGrid input
      used = countUsed grid
      marked = countGroups $ markAllGroups grid

  putStrLn $ "Used sectors: " ++ show used
  putStrLn $ "Groups: " ++ show marked
