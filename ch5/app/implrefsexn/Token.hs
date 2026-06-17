module Token(Token(..), tokenStrList) where

import Prelude hiding(EQ)
import TokenInterface

data Token =
    END_OF_TOKEN
    
  | INTEGER_NUMBER              -- number
  
  | SUB                         -- - ( expr1, expr2 )
  | OPEN_PAREN  | CLOSE_PAREN
  | COMMA

  | ISZERO                      -- zero? ( expr )

  | IF                          -- if expr1 then expr2 else expr3
  | THEN
  | ELSE
  
  | LET                         -- let identifier = expr1 in expr2
  | IN                            
  | EQ
  
  | LETREC                      -- letrec identifier ( identifier )= expr1 in expr2

  | PROC                        -- proc ( identifier ) expr
                                -- (expr1 expr2)

  | IDENTIFIER                  -- identifier
  
  | BEGIN                       -- begin ..;..;.. end
  | END
  | SEMICOLON

  | SET                         -- set
  
  | LIST                        -- list
  | ISNULL                      -- null?
  | CAR                         -- car
  | CDR                         -- cdr

  | TRY                         -- try
  | CATCH                       -- catch
  | RAISE                       -- raise  
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN, "$"),
    
    (INTEGER_NUMBER, "integer_number"),
    
    (SUB, "-"),
    (OPEN_PAREN, "("),
    (CLOSE_PAREN, ")"),
    (COMMA, ","),

    (ISZERO, "zero?"),

    (IF, "if"), 
    (THEN, "then"), 
    (ELSE, "else"), 

    (IDENTIFIER, "identifier"),
    
    (LET, "let"), 
    (IN, "in"), 
    (EQ, "="),

    (LETREC, "letrec"),
    
    (PROC, "proc"),

    (LIST,   "list"),
    (ISNULL, "null?"),
    (CAR,    "car"),
    (CDR,    "cdr"),
    
    (BEGIN, "begin"),
    (END, "end"),
    (SEMICOLON, ";"),

    (SET, "set"),

    (TRY,    "try"),
    (CATCH,  "catch"),
    (RAISE,  "raise")    
  ] 

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

findStr str [] = Nothing
findStr str ((tok,str_):list)
  | str == str_ = Just tok
  | otherwise   = findStr str list

instance TokenInterface Token where
  -- toToken str   =
  --   case findStr str tokenStrList of
  --     Nothing  -> error ("toToken: " ++ str)
  --     Just tok -> tok
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
  

  isEOT END_OF_TOKEN = True
  isEOT _            = False  
