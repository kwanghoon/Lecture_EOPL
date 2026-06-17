module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token
import qualified Data.Map as Map

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
        ("null\\?" , mkFn ISNULL),

        -- ("if"      , mkFn IF),
        -- ("then"    , mkFn THEN),
        -- ("else"    , mkFn ELSE),
        
        -- ("letrec"  , mkFn LETREC),

        -- ("let"     , mkFn LET),
        -- ("in"      , mkFn IN),
        ("\\="     , mkFn EQ),
        
        -- ("proc"    , mkFn PROC),

        -- ("begin"    , mkFn BEGIN),
        -- ("end"    , mkFn END),
        (";"    , mkFn SEMICOLON),

        -- ("set"    , mkFn SET),

        -- ("list"  , mkFn LIST),


        ("[a-zA-Z][a-zA-Z0-9]*"    , keywordOrIdentifier)
      ]
  } 

keywordMap :: Map.Map String Token
keywordMap = Map.fromList (map swap tokenStrList)
  where swap (a,b) = (b,a)

keywordOrIdentifier :: Monad m => String -> m (Maybe Token)
keywordOrIdentifier text = 
  case Map.lookup text keywordMap of
    Nothing -> return $ Just IDENTIFIER
    Just tok -> return $ Just tok