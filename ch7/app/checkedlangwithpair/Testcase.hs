module Testcase where

import Expr(Type)

-- for testing the type checker
type TestCaseName = String
type ExprText = String

data TestCase =
    -- e.g., TDTC "simple-let-1" "let x = 3 in x" (Just TyInt)
    -- Will be deprecasted in the future
    TDTC TestCaseName ExprText (Maybe Type)

    -- e.g., TYCK "TYCHK_simple-let-1.checked" (Just TyInt)
  | TYCK TestCaseName (Maybe Type)

    -- e.g., RUN "simple-let-1" (Just "11")
  | RUN TestCaseName (Maybe String)         

tcname (TDTC tcname _ _) = tcname
tcname (TYCK tcname _)   = tcname
tcname (RUN tcname _)    = tcname

data TestSuite =
  TestSuite [ TestCase ]
