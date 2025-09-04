
-- The syntax is based on the implicitrefs language, and
-- the semantics is based on the one for the continuation-based language.

module Parser where

import CommonParserUtil
import Token
import Expr

import Data.List (unfoldr)


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

      rule "Expression -> string"
        (\rhs -> let raw = init (tail (getText rhs 1))
                     decoded = decodeEscapes raw
                 in return $ PETExp (Str_Exp decoded)),

      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ PETExp (Binary_Exp Diff (expFrom (get rhs 3)) (expFrom (get rhs 5)))),

      rule "Expression -> ( Expression - Expression )"
        (\rhs -> return $ PETExp (Binary_Exp Diff (expFrom (get rhs 2)) (expFrom (get rhs 4)))),

      rule "Expression -> ( Expression + Expression )"
        (\rhs -> return $ PETExp (Binary_Exp Add (expFrom (get rhs 2)) (expFrom (get rhs 4)))),

      rule "Expression -> ( Expression * Expression )"
        (\rhs -> return $ PETExp (Binary_Exp Mul (expFrom (get rhs 2)) (expFrom (get rhs 4)))),
      
      rule "Expression -> random Expression Expression"
        (\rhs -> return $ PETExp (Binary_Exp Random (expFrom (get rhs 2)) (expFrom (get rhs 3)))),

      rule "Expression -> ( Expression mod Expression )"
        (\rhs -> return $ PETExp (Binary_Exp Mod (expFrom (get rhs 2)) (expFrom (get rhs 4)))),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ PETExp (If_Exp (expFrom (get rhs 2)) (expFrom (get rhs 4)) (expFrom (get rhs 6)))),

      rule "Expression -> identifier" (\rhs -> return $ PETExp (Var_Exp (getText rhs 1))),

      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ PETExp (Let_Exp (getText rhs 2) (expFrom (get rhs 4)) (expFrom (get rhs 6)))),

      rule "Expression -> letrec ArbiNumberOfUnaryProcs in Expression"
        (\rhs -> let Letrec_Exp recbinds _ = (expFrom (get rhs 2)) in
                   return $ PETExp (Letrec_Exp recbinds (expFrom (get rhs 4)))),

      rule "ArbiNumberOfUnaryProcs -> identifier OptIdentifier ( identifier ) = Expression"
        (\rhs -> return $ PETExp (Letrec_Exp [ (getText rhs 1, optIdFrom (get rhs 2), getText rhs 4, (expFrom (get rhs 7))) ] undefined)),

      rule "ArbiNumberOfUnaryProcs -> identifier OptIdentifier ( identifier ) = Expression ArbiNumberOfUnaryProcs"
        (\rhs -> let recbind = (getText rhs 1, optIdFrom (get rhs 2), getText rhs 4, (expFrom (get rhs 7)))
                     Letrec_Exp theRest body = (expFrom (get rhs 8))
                 in  return $ PETExp (Letrec_Exp (recbind:theRest) body)),

      rule "Expression -> proc OptIdentifier ( identifier ) Expression"
        (\rhs -> return $ 
            case optIdFrom (get rhs 2) of
              Nothing -> 
                PETExp (Proc_Exp Nothing (getText rhs 4) (expFrom (get rhs 6)))
              Just ('@':roleVar) ->
                PETExp (Proc_Exp Nothing roleVar (Proc_Exp (Just roleVar) (getText rhs 4) (expFrom (get rhs 6))))
              Just actorName ->
                PETExp (Proc_Exp (Just actorName) (getText rhs 4) (expFrom (get rhs 6)))),

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

      rule "Expression -> print ( PrintExpression )"
        (\rhs -> do let exprs = expListFrom (get rhs 3)
                    return $ PETExp $
                      case exprs of
                        [single] -> Unary_Exp Print single
                        _        -> Block_Exp (map (Unary_Exp Print) exprs)),

      rule "PrintExpression -> Expression"
        (\rhs -> return $ PETExpList [expFrom (get rhs 1)]),

      rule "PrintExpression -> PrintExpression ++ PrintExpression"
        (\rhs -> let PETExpList es1 = get rhs 1
                     PETExpList es2 = get rhs 3
                 in return $ PETExpList (es1 ++ es2)),

      rule "Expression -> read ( )"
        (\rhs -> return $ PETExp (Unary_Exp Read (Const_Exp 0))), -- dummy Exp

      rule "Expression -> readInt ( )"
        (\rhs -> return $ PETExp (Unary_Exp ReadInt (Const_Exp 0))), -- dummy Exp        

      rule "Expression -> ( Expression == Expression )"
        (\rhs -> return $ PETExp (Comp_Exp Eq (expFrom (get rhs 2)) (expFrom (get rhs 4)))),

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

      -- rule "Expression -> spawn ( Expression )"
      --   (\rhs -> return $ PETExp (Spawn_Exp (expFrom (get rhs 3)))),

      -- Tuples
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
        (\rhs -> return $ PETIdList (getText rhs 1 : idListFrom (get rhs 3))),
      
      -- Tuple Append
      rule "Expression -> append ( identifier , Expression )"
        (\rhs -> return $ PETExp (Append_Exp (getText rhs 3) (expFrom (get rhs 5)))),

      rule "OptIdentifier -> "
        (\_ -> return $ PETOptIdentifier Nothing),

      rule "OptIdentifier -> identifier"
        (\rhs -> return $ PETOptIdentifier (Just (getText rhs 1))),

      -- zkp
      rule "Expression -> powMod Expression Expression Expression"
        (\rhs -> return $ PETExp (PowMod_Exp (expFrom (get rhs 2)) (expFrom (get rhs 3)) (expFrom (get rhs 4))))
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
  | PETOptIdentifier { optIdFrom :: Maybe Identifier }
  | PETExpList { expListFrom :: [Exp] }
  deriving (Show)


decodeEscapes :: String -> String
decodeEscapes [] = []
decodeEscapes ('\\':'n':xs)  = '\n' : decodeEscapes xs
decodeEscapes ('\\':'t':xs)  = '\t' : decodeEscapes xs
decodeEscapes ('\\':'r':xs)  = '\r' : decodeEscapes xs
decodeEscapes ('\\':'\\':xs) = '\\' : decodeEscapes xs
decodeEscapes ('\\':'"':xs)  = '\"' : decodeEscapes xs
decodeEscapes ('\\':x:xs)    = x : decodeEscapes xs
decodeEscapes (x:xs)         = x : decodeEscapes xs
