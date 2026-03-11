module Main where

import Expr
import TypeCheck
import TypeCheckerTest
import Testcase

import CommonParserUtil
import TokenInterface
import Lexer
import Parser

import Interp (value_of_program)

import Data.Either(isLeft)
import Test.Hspec

main :: IO ()
main = 
  hspec $ do 
    describe "checkedimplicitrefslang:" $ do
      let atdir f = f
      let TestSuite typechecker_tests' = typechecker_tests

      mapM_ 
       (\tdtcArg -> (it (atdir(tcname tdtcArg)) $ do doTest tdtcArg))
       typechecker_tests'
  
fileNameToProgram :: String -> IO Program
fileNameToProgram tcname =
  do let atdir f = "./app/checkedimplicitrefslang/examples/" ++ f
     exprText <- readFile (atdir tcname)
     textToProgram exprText

textToProgram :: String -> IO Program
textToProgram exprText =
  do programAst <-
          parsing False
              parserSpec ((), 1, 1, exprText)
              (aLexer lexerSpec)
              (fromToken (endOfToken lexerSpec))

     return (expFrom programAst)

doTest (TDTC tcname exprText maybeResult) =
  do prog <- textToProgram exprText
     typeTest tcname prog maybeResult

doTest (TYCK tcname maybeResult) = 
  do prog <- fileNameToProgram tcname
     typeTest tcname prog maybeResult 

doTest (RUN tcname maybeResult) =
  do prog <- fileNameToProgram tcname
     runTest tcname prog maybeResult 

typeTest tcname prog maybeResult =
  do  case maybeResult of 
        Just ty' ->
          do eitherTyOrErr <- typeCheck prog
             case eitherTyOrErr of
              Left errMsg -> eitherTyOrErr `shouldBe`  Right ty'                   
              Right ty -> 
                if equalType ty ty'
                    then eitherTyOrErr `shouldBe` Right ty
                    else eitherTyOrErr `shouldBe` Right ty'
        Nothing ->
          do eitherTyOrErr <- typeCheck prog
             case eitherTyOrErr of
              Left errMsg -> eitherTyOrErr `shouldBe` Left errMsg
              Right ty -> eitherTyOrErr `shouldBe` Left "some type error message"

runTest tcname prog maybeResult =
  do  -- do not typecheck, just run the program
      case maybeResult of
        Just resultStr -> do let result = value_of_program prog
                             show result `shouldBe` resultStr
        Nothing -> (return $ value_of_program prog) 
                     `shouldThrow` anyException

