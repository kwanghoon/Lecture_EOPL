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
        
        ("->"      , mkFn ARROW),
        
        ("\\-"     , mkFn SUB),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\,"     , mkFn COMMA),
        
        (":"       , mkFn COLON),
        
        ("zero\\?" , mkFn ISZERO),

        ("if"      , mkFn IF),
        ("then"    , mkFn THEN),
        ("else"    , mkFn ELSE),
        
        ("letrec"  , mkFn LETREC),

        ("int"     , mkFn TYINT),
        ("bool"    , mkFn TYBOOL),
        
        ("\\*", mkFn STAR),
        ("newpair"     , mkFn NEWPAIR),
        ("unpair",  mkFn UNPAIR),
        ("pairof",  mkFn TYPAIROF),

        ("let"     , mkFn LET),
        ("in"      , mkFn IN),
        ("\\="     , mkFn EQ),
        
        ("proc"    , mkFn PROC),

        
        ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  } 
