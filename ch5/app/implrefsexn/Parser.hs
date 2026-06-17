module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token PET IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Expression'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "Expression' -> Expression" (\rhs -> return $ get rhs 1),

      rule "Expression -> integer_number"
        (\rhs -> return $ fromExp $ Const_Exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ fromExp $ Const_Exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ fromExp $ Diff_Exp (expFrom (get rhs 3)) (expFrom (get rhs 5))),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ fromExp $ If_Exp (expFrom (get rhs 2)) (expFrom (get rhs 4)) (expFrom (get rhs 6))),

      rule "Expression -> identifier" (\rhs -> return $ fromExp $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ fromExp $ Let_Exp (getText rhs 2) (expFrom (get rhs 4)) (expFrom (get rhs 6))),

      rule "Expression -> letrec identifier ( identifier ) = Expression in Expression"
        (\rhs -> return $ fromExp $ Letrec_Exp (getText rhs 2) (getText rhs 4) (expFrom (get rhs 7)) (expFrom (get rhs 9))),

      rule "Expression -> proc ( identifier ) Expression"
        (\rhs -> return $ fromExp $ Proc_Exp (getText rhs 3) (expFrom (get rhs 5))),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ fromExp $ Call_Exp (expFrom (get rhs 2)) (expFrom (get rhs 3))),

      rule "Expression -> begin ExpressionList end"
        (\rhs -> return $ fromExp $ Block_Exp (expListFrom (get rhs 2))),

      rule "Expression -> set identifier = Expression"
        (\rhs -> return $ fromExp $ Set_Exp (getText rhs 2) (expFrom (get rhs 4))),
      
      rule "Expression -> list ( NumberList )"
        (\rhs -> return $ get rhs 3),      

      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ fromExp $ Unary_Exp IsZero (expFrom (get rhs 3))),
      
      rule "Expression -> null? ( Expression )"
        (\rhs -> return $ fromExp $ Unary_Exp IsNull (expFrom (get rhs 3))),

      rule "Expression -> car ( Expression )"
        (\rhs -> return $ fromExp $ Unary_Exp Car (expFrom (get rhs 3))),

      rule "Expression -> cdr ( Expression )"
        (\rhs -> return $ fromExp $ Unary_Exp Cdr (expFrom (get rhs 3))),

      rule "Expression -> try Expression catch ( identifier ) Expression"
        (\rhs -> return $ fromExp $ Try_Exp (expFrom (get rhs 2)) (getText rhs 5) (expFrom (get rhs 7))),

      rule "Expression -> raise Expression"
        (\rhs -> return $ fromExp $ Raise_Exp (expFrom (get rhs 2))),

      rule "NumberList -> integer_number"
        (\rhs -> return $ fromExp $ Const_List_Exp [read (getText rhs 1) :: Int]),

      rule "NumberList -> integer_number , NumberList"
        (\rhs ->
           let num           = read (getText rhs 1) :: Int
               Const_List_Exp nums = expFrom (get rhs 3 )
           in  return $ fromExp $ Const_List_Exp (num : nums)),

      rule "ExpressionList -> Expression"
        (\rhs -> return $ fromExpList $ [ expFrom (get rhs 1) ]),

      rule "ExpressionList -> Expression ; ExpressionList"
        (\rhs -> return $ fromExpList $ (expFrom (get rhs 1) : expListFrom (get rhs 3)))                  
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_implrefsexn.txt",
    gotoTblFile    = "goto_table_implrefsexn.txt",
    grammarFile    = "prod_rules_implrefsexn.txt",
    parserSpecFile = "mygrammar_implrefsexn.grm",
    genparserexe   = "yapb-exe"
  }


