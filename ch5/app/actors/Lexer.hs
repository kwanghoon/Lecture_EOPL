{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class(lift)

mkFn :: Token -> LexAction Token IO ()
mkFn tok = \text -> return $ Just tok

skip :: LexAction Token IO ()
skip = \text -> return $ Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ ("[ \t\n]" , skip),
        ("//" ++ "([^\n])*" ++ "[\n]", skip),

        ("[0-9]+"  , mkFn INTEGER_NUMBER),

        --("\"[^\"]*\""      , mkFn STRING),
        --("\""      , init_string_literal),
        ("\"([^\"\\\\]|\\\\.)*\"", mkFn STRING),
        
        ("\\-"     , mkFn SUB),
        ("\\*"     , mkFn TIMES),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\,"     , mkFn COMMA),
        ("\\+\\+"  , mkFn PLUSPLUS),
        ("\\+"     , mkFn PLUS),
        ("\\=\\="  , mkFn EQEQ),
        
        ("\\="     , mkFn EQ),
        
        (";"       , mkFn SEMICOLON),

        ("\\["     , mkFn OPEN_BRACKET),
        ("\\]"     , mkFn CLOSE_BRACKET),

        -- identifiers ending with a symbol
        ("zero\\?" , mkFn ISZERO),
        ("null\\?"   , mkFn ISNULL),
        ("actor\\?"  , mkFn EQACTOR),
        
        ("[@]*[_a-zA-Z][_a-zA-Z0-9]*"    , keywordOrIdentifier)
      ]
  } 

keywordOrIdentifier text =
  case lookup text keywords of
    Nothing  -> mkFn IDENTIFIER text
    Just tok -> mkFn tok text

keywords =
  [ ("if",     IF)
  , ("then",   THEN)
  , ("else",   ELSE)
  , ("letrec", LETREC)
  , ("let",    LET)
  , ("proc",   PROC)
  , ("begin",  BEGIN)
  , ("end",    END)
  , ("set",    SET)
  , ("spawn",  SPAWN)
  , ("yield",  YIELD)
  , ("mutex",  MUTEX)
  , ("wait",   WAIT)
  , ("signal", SIGNAL)
  , ("car",    CAR)
  , ("cdr",    CDR)
  , ("print",  PRINT)
  , ("read",   READ)
  , ("readInt",   READINT)
  , ("in",     IN)
  , ("send",   SEND)
  , ("ready",  READY)
  , ("new",    NEW)
  , ("log",    LOG)
  , ("append", APPEND)

  , ("powMod", POWMOD)
  , ("random", RANDOM)
  , ("mod",    MOD)
  ]
  
-- Invariant: text = "/*..."  
multiLineCommentBegin :: LexAction Token IO ()          -- String -> Maybe Token
multiLineCommentBegin = \text0 -> -- /*
  --trace ("multiLineCommentBegin" ++ text0) $
    do  (state_parm_, line, col, text) <- ST.get
        let (newLine, newCol, newText) = mlc (tail (tail text)) line (col+2)
        -- lift $ putStrLn text0
        -- lift $ putStrLn (show line ++ ", " ++ show col ++ ", " ++ text)
        -- lift $ putStrLn (show newLine ++ ", " ++ show newCol ++ ", " ++ newText)
        ST.put (state_parm_, newLine, newCol, newText)
        return Nothing

  where
    mlc [] line col = (line, col, [])
    mlc ('*':'/':text) line col = (line, col+2, text)
    mlc ('\n':text) line col = mlc text (line+1) col
    mlc ('\r':text) line col = mlc text (line+1) col
    mlc (_:text) line col = mlc text line (col+1)

init_string_literal :: LexAction Token IO ()          -- String -> Maybe Token
init_string_literal = \text0 -> 
  do  (state_parm_, line, col, text) <- ST.get
      (newLine, newCol, newText, newStr) <- isl (tail text) line (col+1) ""
      ST.put (state_parm_, newLine, newCol, newText)
      mkFn STRING ("\"" ++ newStr ++ "\"")  -- return (Just STRING)

  where
    isl [] line col accu = return (line, col, [], accu)
    isl ('\"':text) line col accu = do 
         return (line, col+1, text, reverse accu)
    isl ('\\':ch:text) line col accu = do
         let escapedChar = case ch of
                'n'  -> '\n'
                't'  -> '\t'
                '\\' -> '\\'
                '\"' -> '\"'
                _    -> ch
         isl text line (col+2) (escapedChar : accu)
    isl (ch:text) line col accu = do
         --isl text line (col+1) (ch : accu)
         let (newLine, newCol) = if ch == '\n' then (line + 1, 1) else (line, col + 1)
         isl text newLine newCol (ch : accu)