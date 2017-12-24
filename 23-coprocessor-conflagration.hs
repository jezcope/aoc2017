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

type Register = Char
type Value = Int
type Argument = Either Value Register
data Instruction = Set Register Argument
                 | Sub Register Argument
                 | Mul Register Argument
                 | Jnz Argument Argument
  deriving Show
type Program = V.Vector Instruction
data Result = Cont | Halt deriving (Eq, Show)

type Registers = M.Map Char Int
data Machine = Machine { dRegisters :: Registers
                       , dPtr :: !Int
                       , dMulCount :: !Int
                       , dProgram :: Program }

instance Show Machine where
  show d = show (dRegisters d) ++ " @" ++ show (dPtr d) ++ " ×" ++ show (dMulCount d)

defaultMachine = Machine M.empty 0 0 V.empty

type MachineState = State Machine

program :: GenParser Char st Program
program = do
  instructions <- endBy instruction eol
  return $ V.fromList instructions
  where
    instruction = try (regOp "set" Set) <|> regOp "sub" Sub
                  <|> regOp "mul" Mul <|> jump "jnz" Jnz
    regOp n c = do
      string n >> spaces
      val1 <- oneOf "abcdefgh"
      spaces
      val2 <- regOrVal
      return $ c val1 val2
    jump n c = do
      string n >> spaces
      val1 <- regOrVal
      spaces
      val2 <- regOrVal
      return $ c val1 val2
    regOrVal = register <|> value
    register = do
      name <- lower
      return $ Right name
    value = do
      val <- many $ oneOf "-0123456789"
      return $ Left $ read val
    eol = char '\n'

parseProgram :: String -> Either ParseError Program
parseProgram = parse program ""

getReg :: Char -> MachineState Int
getReg r = do
  st <- get
  return $ M.findWithDefault 0 r (dRegisters st)

putReg :: Char -> Int -> MachineState ()
putReg r v = do
  st <- get
  let current = dRegisters st
      new = M.insert r v current
  put $ st { dRegisters = new }

modReg :: (Int -> Int -> Int) -> Char -> Argument -> MachineState ()
modReg op r v = do
  u <- getReg r
  v' <- getRegOrVal v
  putReg r (u `op` v')
  incPtr

getRegOrVal :: Argument -> MachineState Int
getRegOrVal = either return getReg

addPtr :: Int -> MachineState ()
addPtr n = do
  st <- get
  put $ st { dPtr = n + dPtr st }

incPtr = addPtr 1

execInst :: Instruction -> MachineState ()
execInst (Set reg val) = do
  newVal <- getRegOrVal val
  putReg reg newVal
  incPtr
execInst (Mul reg val) = do
  result <- modReg (*) reg val
  st <- get
  put $ st { dMulCount = 1 + dMulCount st }
  return result
execInst (Sub reg val) = modReg (-) reg val
execInst (Jnz val1 val2) = do
  st <- get
  test <- getRegOrVal val1
  jump <- if test /= 0 then getRegOrVal val2 else return 1
  addPtr jump

execNext :: MachineState Result
execNext = do
  st <- get
  let prog = dProgram st
      p = dPtr st
  if p >= length prog then return Halt else do
    execInst (prog V.! p)
    return Cont

runUntilTerm :: MachineState ()
runUntilTerm = do
  result <- execNext
  unless (result == Halt) runUntilTerm

optimisedCalc :: Int -> Int -> Int -> Int
optimisedCalc a b k = sum $ map (const 1) $ filter notPrime [a,a+k..b]
  where
    notPrime n = any (==0) $ map (mod n) [2..(floor $ sqrt $ fromIntegral n)]

main = do
  input <- getContents  
  case parseProgram input of
    Right prog -> do
      let c = defaultMachine { dProgram = prog }
          (_, c') = runState runUntilTerm c
      putStrLn $ show (dMulCount c') ++ " multiplications made"
      putStrLn $ "Calculation result: " ++ show (optimisedCalc 107900 124900 17)
    Left error -> do
      print error
