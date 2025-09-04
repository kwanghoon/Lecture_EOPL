module Token(Token(..)) where

import Prelude hiding(EQ)
import TokenInterface

data Token =
    END_OF_TOKEN
    
  | INTEGER_NUMBER              -- number
  
  | SUB                         -- - ( expr1, expr2 )
  | PLUS                        -- +
  | TIMES                       -- *
  | MOD                         -- mod

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
  
  | OPEN_BRACKET                -- change: lists ===> [ .. ]
  | CLOSE_BRACKET   
  
  | BEGIN                       -- begin ..;..;.. end
  | END
  | SEMICOLON

  | SET                         -- set

  | SPAWN                       -- spawn
  | YIELD                       -- yield
  | MUTEX                       -- mutex
  | WAIT                        -- wait
  | SIGNAL                      -- signal

  | ISNULL                      -- null?
  | CAR                         -- car
  | CDR                         -- cdr
  | PRINT                       -- print
  | READ                        -- read
  | READINT                     -- readInt
  | PLUSPLUS                    -- ++
  | EQEQ                        -- ==

  | TRY                         -- try
  | CATCH                       -- catch
  | RAISE                       -- raise

  | SEND 
  | READY 
  | NEW
  | EQACTOR

  | LOG
  | STRING

  | APPEND                      -- append

  | POWMOD                      -- powMod
  | RANDOM                      -- random

  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN, "$"),
    
    (INTEGER_NUMBER, "integer_number"),
    (STRING, "string"),
    
    (SUB, "-"),
    (PLUS, "+"),
    (TIMES, "*"),
    (OPEN_PAREN, "("),
    (CLOSE_PAREN, ")"),
    (COMMA, ","),

    (ISZERO, "zero?"),

    (IF, "if"), 
    (THEN, "then"), 
    (ELSE, "else"), 

    (IDENTIFIER, "identifier"),
    (BEGIN, "begin"),
    (END, "end"),
    (SEMICOLON, ";"),

    (SET, "set"),

    (SPAWN,  "spawn"),   -- new feaures
    (YIELD,  "yield"),   -- new feaures
    (MUTEX,  "mutex"),   -- new feaures
    (WAIT,   "wait"),    -- new feaures
    (SIGNAL, "signal"),  -- new feaures
    
    (LET, "let"), 
    (IN, "in"), 
    (EQ, "="),

    (LETREC, "letrec"),
    
    (PROC, "proc"),

    (OPEN_BRACKET, "["),    -- change: lists ==> [ .. ]
    (CLOSE_BRACKET, "]"),   -- 
    
    (ISNULL, "null?"),
    (CAR,    "car"),
    (CDR,    "cdr"),
    (PRINT,  "print"),
    (READ,   "read"),
    (READINT,   "readInt"),
    (PLUSPLUS, "++"),
    (EQEQ,     "=="),

    (TRY,    "try"),
    (CATCH,  "catch"),
    (RAISE,  "raise"),

    (SEND,   "send"),
    (READY,  "ready"),
    (NEW,    "new"),
    (EQACTOR, "actor?"),

    (LOG, "log"),

    (APPEND, "append"),

    (POWMOD, "powMod"),
    (RANDOM, "random"),
    (MOD, "mod")
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
