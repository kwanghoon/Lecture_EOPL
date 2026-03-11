module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Expression'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "Expression' -> Expression" (\rhs -> return $ get rhs 1),

      rule "Expression -> integer_number"
        (\rhs -> return $ toASTExp $ Const_Exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ toASTExp $ Const_Exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ toASTExp $ Diff_Exp (fromASTExp $ get rhs 3) (fromASTExp $ get rhs 5)),

      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ toASTExp $ IsZero_Exp (fromASTExp $ get rhs 3)),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ toASTExp $ If_Exp (fromASTExp $ get rhs 2) (fromASTExp $ get rhs 4) (fromASTExp $ get rhs 6)),

      rule "Expression -> identifier" (\rhs -> return $ toASTExp $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ toASTExp $ Let_Exp (getText rhs 2) (fromASTExp $ get rhs 4) (fromASTExp $ get rhs 6)),

      rule "Expression -> letrec Type identifier ( identifier : Type ) = Expression in Expression"
        (\rhs -> return $ toASTExp $ Letrec_Exp (fromASTType $ get rhs 2) (getText rhs 3) (getText rhs 5) (fromASTType $ get rhs 7) (fromASTExp $ get rhs 10) (fromASTExp $ get rhs 12)),

      rule "Expression -> proc ( identifier : Type ) Expression"
        (\rhs -> return $ toASTExp $ Proc_Exp (getText rhs 3) (fromASTType $ get rhs 5) (fromASTExp $ get rhs 7)),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ toASTExp $ Call_Exp (fromASTExp $ get rhs 2) (fromASTExp $ get rhs 3)),

      rule "Expression -> newpair ( Expression , Expression )"
        (\rhs -> return $ toASTExp $ NewPair_Exp (fromASTExp $ get rhs 3) (fromASTExp $ get rhs 5)),

      rule "Expression -> unpair ( identifier , identifier ) = Expression in Expression"
        (\rhs -> return $ toASTExp $ Unpair_Exp (getText rhs 3) (getText rhs 5) (fromASTExp $ get rhs 8) (fromASTExp $ get rhs 10)),
        
      rule "Type -> int"
        (\rhs -> return $ toASTType $ TyInt),
      
      rule "Type -> bool"
        (\rhs -> return $ toASTType $ TyBool),
      
      rule "Type -> ( Type -> Type )"
        (\rhs -> return $ toASTType $ TyFun (fromASTType (get rhs 2)) (fromASTType (get rhs 4))),

      rule "Type -> pairof Type * Type"
        (\rhs -> return $ toASTType $ TyPair (fromASTType (get rhs 2)) (fromASTType (get rhs 4)))
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_checkedlangwithpair.txt",
    gotoTblFile    = "goto_table_checkedlangwithpair.txt",
    grammarFile    = "prod_rules_checkedlangwithpair.txt",
    parserSpecFile = "mygrammar_checkedlangwithpair.grm",
    genparserexe   = "yapb-exe"
  }


