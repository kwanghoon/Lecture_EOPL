module MainUtil where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Interp
import TypeCheck

import Control.Monad (when)
import System.IO
import System.Environment (getArgs, withArgs)

parser text = do
  astProg <-
    parsing False                            -- parser converting a text-based program
     parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
     (aLexer lexerSpec)
     (fromToken (endOfToken lexerSpec))
  return (fromASTProgram astProg)

parseFile fileName = do
  text <- readFile fileName
  let debugFlag = False
        
  programAst <-
    parsing debugFlag
       parserSpec ((), 1, 1, text)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

  let program = fromASTProgram programAst
  
  return program

run text = do
  val <- runProg text True
  putStrLn (show val)

runProg text bool = do 
  program <- parser text

  if bool then putStrLn (show program) else return ()
  
  let val = value_of_program program      -- interpreter
  return val 

typecheck text = do
  let debugFlag = False
        
  program <- parser text
  
  putStrLn (show program)

  eitherTyOrErr <- typeCheck program
  case eitherTyOrErr of
    Right ty ->
      do putStrLn (show ty)

        --  let val = value_of_program program
        --  putStrLn (show val)

    Left errMsg ->
      do putStrLn errMsg
