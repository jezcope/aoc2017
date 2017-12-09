module Main where

import Test.Hspec
import Data.Function ((&))

countGroups :: String -> Int
countGroups = countGroups' 0 0 False False
  where
    countGroups' score _ _ _ [] = score
    countGroups' score level garbage skip (c:rest)
      | skip      = countGroups' score level garbage False rest
      | c == '!'  = countGroups' score level garbage True rest
      | garbage   = case c of
                      '>' -> countGroups' score level False False rest
                      _   -> countGroups' score level True False rest
      | otherwise = case c of
                      '{' -> countGroups' score (level+1) False False rest
                      '}' -> countGroups' (score+level) (level-1) False False rest
                      ',' -> countGroups' score level False False rest
                      '<' -> countGroups' score level True False rest
                      c   -> error $ "Garbage character found outside garbage: " ++ show c

countGarbage :: String -> Int
countGarbage = countGarbage' 0 False False
  where
    countGarbage' count _ _ [] = count
    countGarbage' count garbage skip (c:rest)
      | skip      = countGarbage' count garbage False rest
      | c == '!'  = countGarbage' count garbage True rest
      | garbage   = case c of
                      '>' -> countGarbage' count False False rest
                      _   -> countGarbage' (count+1) True False rest
      | otherwise = case c of
                      '<' -> countGarbage' count True False rest
                      _   -> countGarbage' count False False rest

runTests = 
  hspec $ do
    describe "countGroups" $ do
      it "counts valid groups" $ do
        countGroups "{}" `shouldBe` 1
        countGroups "{{{}}}" `shouldBe` 6
        countGroups "{{{},{},{{}}}}" `shouldBe` 16
        countGroups "{{},{}}" `shouldBe` 5

      it "ignores garbage" $ do
        countGroups "{<a>,<a>,<a>,<a>}" `shouldBe` 1
        countGroups "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9

      it "skips marked characters" $ do
        countGroups "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
        countGroups "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
      

    describe "countGarbage" $ do
      it "counts garbage characters" $ do
        countGarbage "<>" `shouldBe` 0
        countGarbage "<random characters>" `shouldBe` 17
        countGarbage "<<<<>" `shouldBe` 3

      it "ignores non-garbage" $ do
        countGarbage "{{},{}}" `shouldBe` 0
        countGarbage "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 8
        
      it "skips marked characters" $ do
        countGarbage "<{!>}>" `shouldBe` 2
        countGarbage "<!!>" `shouldBe` 0
        countGarbage "<!!!>" `shouldBe` 0
        countGarbage "<{o\"i!a,<{i<a>" `shouldBe` 10

main = do
  runTests

  repeat '=' & take 78 & putStrLn

  input <- getContents & fmap (filter (/='\n'))
  putStrLn $ "Found " ++ show (countGroups input) ++ " groups"
  putStrLn $ "Found " ++ show (countGarbage input) ++ " characters garbage"
