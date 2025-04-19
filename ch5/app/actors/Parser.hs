
-- The syntax is based on the implicitrefs language, and
-- the semantics is based on the one for the continuation-based language.

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
        (\rhs -> return $ PETExp (Const_Exp (read (getText rhs 1) :: Int))),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ PETExp (Const_Exp (-(read (getText rhs 2) :: Int)))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ PETExp (Diff_Exp (expFrom (get rhs 3)) (expFrom (get rhs 5)))),

      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ PETExp (If_Exp (expFrom (get rhs 2)) (expFrom (get rhs 4)) (expFrom (get rhs 6)))),

      rule "Expression -> identifier" (\rhs -> return $ PETExp (Var_Exp (getText rhs 1))),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ PETExp (Let_Exp (getText rhs 2) (expFrom (get rhs 4)) (expFrom (get rhs 6)))),

      rule "Expression -> letrec ArbiNumberOfUnaryProcs in Expression"
        (\rhs -> let Letrec_Exp recbinds _ = (expFrom (get rhs 2)) in
                   return $ PETExp (Letrec_Exp recbinds (expFrom (get rhs 4)))),

      rule "ArbiNumberOfUnaryProcs -> identifier ( identifier ) = Expression"
        (\rhs -> return $ PETExp (Letrec_Exp [ (getText rhs 1, getText rhs 3, (expFrom (get rhs 6))) ] undefined)),

      rule "ArbiNumberOfUnaryProcs -> identifier ( identifier ) = Expression ArbiNumberOfUnaryProcs"
        (\rhs -> let recbind = (getText rhs 1, getText rhs 3, (expFrom (get rhs 6)))
                     Letrec_Exp theRest body = (expFrom (get rhs 7))
                 in  return $ PETExp (Letrec_Exp (recbind:theRest) body)),
      
      rule "Expression -> proc ( identifier ) Expression"
        (\rhs -> return $ PETExp (Proc_Exp (getText rhs 3) (expFrom (get rhs 5)))),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ PETExp (Call_Exp (expFrom (get rhs 2)) (expFrom (get rhs 3)))),

      rule "Expression -> begin BlockExpressionList end"
        (\rhs -> return $ get rhs 2),

      rule "BlockExpressionList -> Expression"
        (\rhs -> return $ PETExp (Block_Exp [ expFrom (get rhs 1) ])),

      rule "BlockExpressionList -> Expression ; BlockExpressionList"
        (\rhs -> return $
                   case expFrom (get rhs 3) of
                     Block_Exp exprs -> PETExp (Block_Exp (expFrom (get rhs 1) : exprs))),
        
      rule "Expression -> set identifier = Expression"
        (\rhs -> return $ PETExp (Set_Exp (getText rhs 2) (expFrom (get rhs 4)))),

      rule "Expression -> spawn ( Expression )"    -- key features
        (\rhs -> return $ PETExp (Spawn_Exp (expFrom (get rhs 3)))),

      rule "Expression -> yield ( )"               -- key features
        (\rhs -> return $ PETExp Yield_Exp),
      
      rule "Expression -> mutex ( )"               -- key features
        (\rhs -> return $ PETExp Mutex_Exp),
      
      rule "Expression -> wait ( Expression )"     -- key features
        (\rhs -> return $ PETExp (Wait_Exp (expFrom (get rhs 3)))),
      
      rule "Expression -> signal ( Expression )"   -- key features
        (\rhs -> return $ PETExp (Signal_Exp (expFrom (get rhs 3)))),
      
      rule "Expression -> [ NumberList ]"          -- change: lists => [ ... ]
        (\rhs -> return $ get rhs 2),

      rule "NumberList -> integer_number"
        (\rhs -> return $ PETExp (Const_List_Exp [read (getText rhs 1) :: Int])),

      rule "NumberList -> integer_number , NumberList"
        (\rhs ->
           let num           = read (getText rhs 1) :: Int
               Const_List_Exp nums = expFrom (get rhs 3) 
           in  return $ PETExp (Const_List_Exp (num : nums))),
      
      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ PETExp (Unary_Exp IsZero (expFrom (get rhs 3)))),
      
      rule "Expression -> null? ( Expression )"
        (\rhs -> return $ PETExp (Unary_Exp IsNull (expFrom (get rhs 3)))),

      rule "Expression -> car ( Expression )"
        (\rhs -> return $ PETExp (Unary_Exp Car (expFrom (get rhs 3)))),

      rule "Expression -> cdr ( Expression )"
        (\rhs -> return $ PETExp (Unary_Exp Cdr (expFrom (get rhs 3)))),

      rule "Expression -> print ( Expression )"
        (\rhs -> return $ PETExp (Unary_Exp Print (expFrom (get rhs 3)))),

      rule "Expression -> log string Expression"
        (\rhs -> return $ PETExp (Log_Exp (init (tail (getText rhs 2))) (expFrom (get rhs 3)))),

      -- Actors
      rule "Expression -> send ( SendExpressionList )"
        (\rhs -> return $ (get rhs 3)),

      rule "SendExpressionList -> Expression"
        (\rhs -> return $ PETExp (Send_Exp [ expFrom (get rhs 1) ])),

      rule "SendExpressionList -> Expression , SendExpressionList"
        (\rhs ->
            case expFrom (get rhs 3) of
              Send_Exp exprs -> return $ PETExp (Send_Exp (expFrom (get rhs 1) : exprs))),

      rule "Expression -> ready ( Expression )"
        (\rhs -> return $ PETExp (Ready_Exp (expFrom (get rhs 3)))),
      
      rule "Expression -> new ( Expression )"
        (\rhs -> return $ PETExp (New_Exp (expFrom (get rhs 3)))),

      rule "Expression -> actor? ( Expression , Expression )"
        (\rhs -> return $ PETExp (Eq_Actor_Exp (expFrom (get rhs 3)) (expFrom (get rhs 5)))),

      rule "Expression -> ( TupleExpressionList )"
        (\rhs -> return $ get rhs 2),

      rule "TupleExpressionList -> "
        (\rhs -> return $ PETExp (Tuple_Exp [])),

      rule "TupleExpressionList -> Expression"
        (\rhs -> return $ PETExp (Tuple_Exp [expFrom (get rhs 1)])),
      
      rule "TupleExpressionList -> Expression , TupleExpressionList"
        (\rhs -> return $
                   case expFrom (get rhs 3) of
                     Tuple_Exp exprs -> PETExp (Tuple_Exp (expFrom (get rhs 1) : exprs))),

      -- Tuple Let Binding
      rule "Expression -> let ( IdentifierList ) = Expression in Expression"
        (\rhs -> return $ PETExp (LetTuple_Exp (idListFrom (get rhs 3)) (expFrom (get rhs 6)) (expFrom (get rhs 8)))),

      -- IdentifierList :: [String]
      rule "IdentifierList -> "
        (\rhs -> return $ PETIdList []),

      rule "IdentifierList -> identifier"
        (\rhs -> return $ PETIdList [getText rhs 1]),
      
      rule "IdentifierList -> identifier , IdentifierList"
        (\rhs -> return $ PETIdList (getText rhs 1 : idListFrom (get rhs 3)) )
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_actorslang.txt",
    gotoTblFile    = "goto_table_actorslang.txt",
    grammarFile    = "prod_rules_actorslang.txt",
    parserSpecFile = "mygrammar_actorslang.grm",
    genparserexe   = "yapb-exe"
  }

data PET =
    PETExp { expFrom :: Exp } 
  | PETIdList { idListFrom :: [String] }
  deriving (Show)