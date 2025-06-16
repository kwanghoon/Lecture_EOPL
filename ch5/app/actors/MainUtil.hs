module MainUtil where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Interp
import Scheduler

import Control.Monad (when)
import System.IO
import System.Environment (getArgs, withArgs)


parser text = do
    parsing True                            -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

runProg text bool = do 
  pet <- parser text
  let expression = expFrom pet

  if bool then putStrLn (show expression) else return ()
  
  let val = value_of_program expression     -- interpreter
  return val
--  putStrLn (show val)
  
run text = runProg text True