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
    describe "checkedlangwithpair" $ do
      let atdir f = "checkedlangwithpair:" ++ f
      let TestSuite typechecker_tests' = typechecker_tests

      mapM_ 
       (\tdtcArg -> (it (atdir(tcname tdtcArg)) $ do doTest tdtcArg))
       typechecker_tests'
  
fileNameToExpr :: String -> IO Exp
fileNameToExpr tcname =
  do let atdir f = "./app/checkedlangwithpair/examples/" ++ f
     exprText <- readFile (atdir tcname)
     textToExpr exprText

textToExpr :: String -> IO Exp
textToExpr exprText =
  do expressionAst <-
          parsing False
              parserSpec ((), 1, 1, exprText)
              (aLexer lexerSpec)
              (fromToken (endOfToken lexerSpec))

     return (fromASTExp expressionAst)

doTest (TDTC tcname exprText maybeResult) =
  do expression <- textToExpr exprText
     typeTest tcname expression maybeResult

doTest (TYCK tcname maybeResult) = 
  do expr <- fileNameToExpr tcname
     typeTest tcname expr maybeResult 

doTest (RUN tcname maybeResult) =
  do expr <- fileNameToExpr tcname
     runTest tcname expr maybeResult 

typeTest tcname expression maybeResult =
  do  -- Just add the type of x!
      case maybeResult of 
        Just ty' ->
          do eitherTyOrErr <- typeCheck (Let_Exp "x" (Const_Exp 1) expression)
             case eitherTyOrErr of
              Left errMsg -> Left errMsg `shouldBe`  Right ty'                   
              Right ty -> 
                if equalType ty ty'
                    then eitherTyOrErr `shouldBe` Right ty
                    else eitherTyOrErr `shouldBe` Right ty'
        Nothing ->
          do eitherTyOrErr <- typeCheck (Let_Exp "x" (Const_Exp 1) expression)
             case eitherTyOrErr of
              Left errMsg -> eitherTyOrErr `shouldBe` Left errMsg
              Right ty -> eitherTyOrErr `shouldBe` Left "some type error message"

runTest tcname expression maybeResult =
  do  -- do not typecheck, just run the program
      case maybeResult of
        Just resultStr -> do let result = value_of_program expression
                             show result `shouldBe` resultStr
        Nothing -> (return $ value_of_program expression) 
                     `shouldThrow` anyException
