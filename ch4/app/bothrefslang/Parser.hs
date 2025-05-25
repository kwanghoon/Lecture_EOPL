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

      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ fromExp $ IsZero_Exp (expFrom (get rhs 3))),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ fromExp $ If_Exp (expFrom (get rhs 2)) (expFrom (get rhs 4)) (expFrom (get rhs 6))),

      rule "Expression -> identifier" (\rhs -> return $ fromExp $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ fromExp $ Let_Exp (getText rhs 2) (expFrom (get rhs 4)) (expFrom (get rhs 6))),

      rule "Expression -> letmutable identifier = Expression in Expression"
        (\rhs -> return $ fromExp $ LetMutable_Exp (getText rhs 2) (expFrom (get rhs 4)) (expFrom (get rhs 6))),

      rule "Expression -> letrec LetRecBindings in Expression"
        (\rhs -> return $ fromExp $ Letrec_Exp (idIdExpListFrom (get rhs 2)) (expFrom (get rhs 4))),

      rule "Expression -> proc ( identifier ) Expression"
        (\rhs -> return $ fromExp $ Proc_Exp (getText rhs 3) (expFrom (get rhs 5))),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ fromExp $ Call_Exp (expFrom (get rhs 2)) (expFrom (get rhs 3))),

      rule "Expression -> begin ExpressionList end"
        (\rhs -> return $ fromExp $ Block_Exp (expListFrom (get rhs 2))),

      rule "Expression -> set identifier = Expression"
        (\rhs -> return $ fromExp $ Set_Exp (getText rhs 2) (expFrom (get rhs 4))),

      rule "LetRecBindings -> identifier ( identifier ) = Expression"
        (\rhs -> return $ fromIdIdExpList [(getText rhs 1, getText rhs 3, expFrom (get rhs 6))]),

      rule "LetRecBindings -> identifier ( identifier ) = Expression LetRecBindings"
        (\rhs -> return $ fromIdIdExpList ((getText rhs 1, getText rhs 3, expFrom (get rhs 6)) : idIdExpListFrom (get rhs 7))),

      rule "ExpressionList -> Expression"
        (\rhs -> return $ fromExpList $ [ expFrom (get rhs 1) ]),

      rule "ExpressionList -> Expression ; ExpressionList"
        (\rhs -> return $ fromExpList $ (expFrom (get rhs 1) : expListFrom (get rhs 3)))        
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_bothrefslang.txt",
    gotoTblFile    = "goto_table_bothrefslang.txt",
    grammarFile    = "prod_rules_bothrefslang.txt",
    parserSpecFile = "mygrammar_bothrefslang.grm",
    genparserexe   = "yapb-exe"
  }


