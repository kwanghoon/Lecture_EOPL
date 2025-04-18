module Main where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Interp
import Comp
import Actors.Expr(pprint)

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
     _ -> do _ <- doProcess True fileName
             _main args


doProcess verbose fileName = do
   text <- readFile fileName
   let debugFlag = False
         
   expression <-
     parsing debugFlag
        parserSpec ((), 1, 1, text)
        (aLexer lexerSpec)
        (fromToken (endOfToken lexerSpec))
  
   putStrLn (show expression)
   putStrLn ""

   let actorExpr = compMain expression
   putStrLn (show actorExpr)
   putStrLn ""

   putStrLn (pprint actorExpr)
   

--    let val = value_of_program expression
--    putStrLn (show val)

-- > parser text = do
-- >     parsing False                            -- parser converting a text-based program
-- >        parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
-- >        (aLexer lexerSpec)
-- >        (fromToken (endOfToken lexerSpec))
-- >

-- > run text = do 
-- >   expression <- parser text
-- > 
-- >   putStrLn (show expression)
-- >
-- >   let val = value_of_program expression      -- interpreter
-- >   putStrLn (show val)
