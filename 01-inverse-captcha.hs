module Main where

import qualified Data.Text as T
import Data.Char (ord)

strip = T.unpack . T.strip . T.pack

readDigit :: Char -> Int
readDigit x = ord x - ord '0'

inverseCaptcha :: String -> Int
inverseCaptcha (d:ds) = inverseCaptcha' $ (d:ds) ++ [d]
  where
    inverseCaptcha' (d0:d1:ds) | d0 == d1  = readDigit d0 + inverseCaptcha' (d1:ds)
                               | otherwise = inverseCaptcha' (d1:ds)
    inverseCaptcha' _ = 0

inverseCaptchaAlt :: String -> Int
inverseCaptchaAlt ds = inverseCaptchaAlt' ds $ drop (length ds `div` 2) $ cycle ds
  where
    inverseCaptchaAlt' (d:ds) (e:es) | d == e    = readDigit d + inverseCaptchaAlt' ds es
                                     | otherwise = inverseCaptchaAlt' ds es
    inverseCaptchaAlt' [] _ = 0

main = do
  input <- getContents
  putStrLn $ show $ inverseCaptcha $ strip input
  putStrLn $ show $ inverseCaptchaAlt $ strip input
