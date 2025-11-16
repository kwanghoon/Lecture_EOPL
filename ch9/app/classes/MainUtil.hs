module MainUtil where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Interp
import EnvStore(ExpVal, initialize_class_env)

import Control.Monad (when)
import System.IO
import System.Environment (getArgs, withArgs)

parser :: String -> IO PET
parser text = do
  parsing False                            -- parser converting a text-based program
     parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
     (aLexer lexerSpec)
     (fromToken (endOfToken lexerSpec))

parseFile fileName = do
  text <- readFile fileName
  let debugFlag = False

  tree <-
    parsing debugFlag
      parserSpec ((),1,1,text)
      (aLexer lexerSpec)
      (fromToken (endOfToken lexerSpec))

  let program = programFrom tree

  print program

run :: String -> IO ()
run text = do
  val <- runProg text True
  putStrLn (show val)

runProg :: String -> Bool -> IO ExpVal
runProg text bool = do 
  _program <- parser text
  let program = programFrom _program

  if bool then putStrLn (show program) else return ()
  
  let val = value_of_program program      -- interpreter
  return val 

classEnv :: String -> IO ()
classEnv fileName = do 
  text <- readFile fileName
  _program <- parser text
  let Program classDecls _ = programFrom _program
  let init_class_Env = initialize_class_env classDecls
  putStrLn (show init_class_Env)