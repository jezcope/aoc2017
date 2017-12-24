{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Loops
import Text.ParserCombinators.Parsec hiding (State)
import Debug.Trace

data CoproVal = Reg Char | Val Int deriving Show
-- type CoproQueue = [Int]
data CoproInstruction =  Set CoproVal CoproVal
                       | Sub CoproVal CoproVal
                       | Mul CoproVal CoproVal
                       | Jnz CoproVal CoproVal
                        deriving Show
type CoproProgram = V.Vector CoproInstruction

type CoproRegisters = M.Map Char Int
data Copro = Copro { dRegisters :: CoproRegisters
                 , dPtr :: Int
                 , dMulCount :: Int
                 , dProgram :: CoproProgram }

instance Show Copro where
  show d = show (dRegisters d) ++ " @" ++ show (dPtr d) ++ " ×" ++ show (dMulCount d)

defaultCopro = Copro M.empty 0 0 V.empty

type CoproState = State Copro

program :: GenParser Char st CoproProgram
program = do
  instructions <- endBy instruction eol
  return $ V.fromList instructions
  where
    instruction = try (twoArg "set" Set) <|> twoArg "sub" Sub
                  <|> twoArg "mul" Mul <|> twoArg "jnz" Jnz
    twoArg n c = do
      string n >> spaces
      val1 <- regOrVal
      spaces
      val2 <- regOrVal
      return $ c val1 val2
    regOrVal = register <|> value
    register = do
      name <- lower
      return $ Reg name
    value = do
      val <- many $ oneOf "-0123456789"
      return $ Val $ read val
    eol = char '\n'

parseProgram :: String -> Either ParseError CoproProgram
parseProgram = parse program ""

getReg :: Char -> CoproState Int
getReg r = do
  st <- get
  return $ M.findWithDefault 0 r (dRegisters st)

putReg :: Char -> Int -> CoproState ()
putReg r v = do
  st <- get
  let current = dRegisters st
      new = M.insert r v current
  put $ st { dRegisters = new }

modReg :: (Int -> Int -> Int) -> Char -> CoproVal -> CoproState Bool
modReg op r v = do
  u <- getReg r
  v' <- getRegOrVal v
  putReg r (u `op` v')
  incPtr
  return False

getRegOrVal :: CoproVal -> CoproState Int
getRegOrVal (Reg r) = getReg r
getRegOrVal (Val v) = return v

addPtr :: Int -> CoproState ()
addPtr n = do
  st <- get
  put $ st { dPtr = n + dPtr st }

incPtr = addPtr 1

execInst :: CoproInstruction -> CoproState Bool
execInst (Set (Reg reg) val) = do
  newVal <- getRegOrVal val
  putReg reg newVal
  incPtr
  return False
execInst (Mul (Reg reg) val) = do
  result <- modReg (*) reg val
  st <- get
  put $ st { dMulCount = 1 + dMulCount st }
  return result
execInst (Sub (Reg reg) val) = modReg (-) reg val
execInst (Jnz val1 val2) = do
  st <- get
  test <- getRegOrVal val1
  jump <- if test /= 0 then getRegOrVal val2 else return 1
  addPtr jump
  return False
execInst x = error $ "execInst not implemented yet for " ++ show x

execNext :: CoproState Bool
execNext = do
  st <- get
  let prog = dProgram st
      p = dPtr st
  if p >= length prog then return True else do
    result <- execInst (prog V.! p)
    return result

runUntilTerm :: CoproState ()
runUntilTerm = do
  terminate <- execNext
  unless terminate runUntilTerm

optimisedCalc :: Int -> Int -> Int -> Int
optimisedCalc a b k = sum $ map (const 1) $ filter notPrime [a,a+k..b]
  where
    notPrime n = any (==0) $ map (mod n) [2..n-1]

main = do
  prog <- fmap (fromRight V.empty . parseProgram) getContents  
  let c = defaultCopro { dProgram = prog }
      (_, c') = runState runUntilTerm c
  putStrLn $ show (dMulCount c') ++ " multiplications made"
  putStrLn $ "Calculation result: " ++ show (optimisedCalc 107900 124900 17)
