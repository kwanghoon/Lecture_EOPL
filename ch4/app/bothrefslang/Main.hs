module Main where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
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

  let expression = expFrom tree

  print expression

  let val = value_of_program expression
  print val
