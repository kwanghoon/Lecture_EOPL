module Main where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import TypeCheck
import Interp

import Control.Monad (when)
import System.IO
import System.Environment (getArgs, withArgs)

main :: IO ()
main =
 do args <- getArgs
    _main args


_main [] = return ()
_main (fileName:args) =
  case fileName of
    "test" -> return ()
    _ -> do _ <- doProcess True fileName
            _main args


doProcess verbose fileName = do
  text <- readFile fileName
  let debugFlag = False

  tree <-
    parsing debugFlag
      parserSpec ((),1,1,text)
      (aLexer lexerSpec)
      (fromToken (endOfToken lexerSpec))

  let program = expFrom tree

  print program
  
  errOrType <- typeCheck program

  case errOrType of
    Left err -> error $ "Type check failure: " ++ err
    Right ty ->
     do putStrLn (show ty)
        let val = value_of_program program
        print val
