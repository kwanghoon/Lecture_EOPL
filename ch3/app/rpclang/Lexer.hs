module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

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
        
        ("[0-9]+"  , mkFn INTEGER_NUMBER),
        
        ("\\-"     , mkFn SUB),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\,"     , mkFn COMMA),
        
        ("zero\\?" , mkFn ISZERO),

        ("if"      , mkFn IF),
        ("then"    , mkFn THEN),
        ("else"    , mkFn ELSE),
        
        ("let"     , mkFn LET),
        ("in"      , mkFn IN),
        ("\\="     , mkFn EQ),
        
        ("proc"      , mkFn PROC),

        ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  } 
