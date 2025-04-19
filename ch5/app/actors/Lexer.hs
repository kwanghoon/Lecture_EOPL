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

        ("\\\"[^\\\"]*\\\"", mkFn STRING),
        
        ("\\-"     , mkFn SUB),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\,"     , mkFn COMMA),
        
        ("\\="     , mkFn EQ),
        
        (";"       , mkFn SEMICOLON),

        ("\\["     , mkFn OPEN_BRACKET),
        ("\\]"     , mkFn CLOSE_BRACKET),

        -- identifiers ending with a symbol
        ("zero\\?" , mkFn ISZERO),
        ("null\\?"   , mkFn ISNULL),
        ("actor\\?"  , mkFn EQACTOR),
        
        ("[_a-zA-Z][_a-zA-Z0-9]*"    , keywordOrIdentifier)
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
  , ("in",     IN)
  , ("send",   SEND)
  , ("ready",  READY)
  , ("new",    NEW)
  , ("log",    LOG)
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