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

data DuetVal = Reg Char | Val Int deriving Show
type DuetQueue = [Int]
data DuetInstruction = Snd DuetVal
                       | Rcv DuetVal
                       | Jgz DuetVal DuetVal
                       | Set DuetVal DuetVal
                       | Add DuetVal DuetVal
                       | Mul DuetVal DuetVal
                       | Mod DuetVal DuetVal
                        deriving Show
type DuetProgram = V.Vector DuetInstruction

type DuetRegisters = M.Map Char Int
data Duet = Duet { dRegisters :: DuetRegisters
                 , dPtr :: Int
                 , dSendCount :: Int
                 , dRcvBuf :: DuetQueue
                 , dSndBuf :: DuetQueue
                 , dProgram :: DuetProgram }

instance Show Duet where
  show d = show (dRegisters d) ++ " @" ++ show (dPtr d) ++ " S" ++ show (dSndBuf d) ++ " R" ++ show (dRcvBuf d)

defaultDuet = Duet M.empty 0 0 [] [] V.empty

type DuetState = State Duet

program :: GenParser Char st DuetProgram
program = do
  instructions <- endBy instruction eol
  return $ V.fromList instructions
  where
    instruction = try (oneArg "snd" Snd) <|> oneArg "rcv" Rcv
                  <|> twoArg "set" Set <|> twoArg "add" Add
                  <|> try (twoArg "mul" Mul)
                  <|> twoArg "mod" Mod <|> twoArg "jgz" Jgz
    oneArg n c = do
      string n >> spaces
      val <- regOrVal
      return $ c val
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

parseProgram :: String -> Either ParseError DuetProgram
parseProgram = parse program ""

getReg :: Char -> DuetState Int
getReg r = do
  st <- get
  return $ M.findWithDefault 0 r (dRegisters st)

putReg :: Char -> Int -> DuetState ()
putReg r v = do
  st <- get
  let current = dRegisters st
      new = M.insert r v current
  put $ st { dRegisters = new }

modReg :: (Int -> Int -> Int) -> Char -> DuetVal -> DuetState Bool
modReg op r v = do
  u <- getReg r
  v' <- getRegOrVal v
  putReg r (u `op` v')
  incPtr
  return False

getRegOrVal :: DuetVal -> DuetState Int
getRegOrVal (Reg r) = getReg r
getRegOrVal (Val v) = return v

addPtr :: Int -> DuetState ()
addPtr n = do
  st <- get
  put $ st { dPtr = n + dPtr st }

incPtr = addPtr 1

send :: Int -> DuetState ()
send v = do
  st <- get
  put $ st { dSndBuf = (dSndBuf st ++ [v]), dSendCount = dSendCount st + 1 }

recv :: DuetState (Maybe Int)
recv = do
  st <- get
  case dRcvBuf st of
    (x:xs) -> do
      put $ st { dRcvBuf = xs }
      return $ Just x
    [] -> return Nothing

execInst :: DuetInstruction -> DuetState Bool
execInst (Set (Reg reg) val) = do
  newVal <- getRegOrVal val
  putReg reg newVal
  incPtr
  return False
execInst (Mul (Reg reg) val) = modReg (*) reg val
execInst (Add (Reg reg) val) = modReg (+) reg val
execInst (Mod (Reg reg) val) = modReg mod reg val
execInst (Jgz val1 val2) = do
  test <- getRegOrVal val1
  jump <- if test > 0 then getRegOrVal val2 else return 1
  addPtr jump
  return False
execInst (Snd val) = do
  v <- getRegOrVal val
  send v
  incPtr
  return False
execInst (Rcv (Reg r)) = do
  v <- recv
  handle v
  where
    handle :: Maybe Int -> DuetState Bool
    handle (Just x) = putReg r x >> incPtr >> return False
    handle Nothing = return True
execInst x = error $ "execInst not implemented yet for " ++ show x

execNext :: DuetState Bool
execNext = do
  st <- get
  let prog = dProgram st
      p = dPtr st
  if p >= length prog then return True else do
    result <- execInst (prog V.! p)
    return result

runUntilWait :: DuetState ()
runUntilWait = do
  waiting <- execNext
  unless waiting runUntilWait

runTwoPrograms :: Duet -> Duet -> (Int, Int)
runTwoPrograms !d0 !d1
  | (null $ dSndBuf d0') && (null $ dSndBuf d1') = (dSendCount d0', dSendCount d1')
  | otherwise = runTwoPrograms d0'' d1''
  where
    (_, d0') = runState runUntilWait d0
    (_, d1') = runState runUntilWait d1
    d0'' = d0' { dSndBuf = [], dRcvBuf = dSndBuf d1' }
    d1'' = d1' { dSndBuf = [], dRcvBuf = dSndBuf d0' }

main = do
  prog <- fmap (fromRight V.empty . parseProgram) getContents  
  let d0 = defaultDuet { dProgram = prog, dRegisters = M.fromList [('p', 0)] }
      d1 = defaultDuet { dProgram = prog, dRegisters = M.fromList [('p', 1)] }
      (send0, send1) = runTwoPrograms d0 d1
  putStrLn $ "Program 0 sent " ++ show send0 ++ " messages"
  putStrLn $ "Program 1 sent " ++ show send1 ++ " messages"
