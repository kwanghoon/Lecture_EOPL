module SystemConstants where

import Expr(Exp(..), Identifier)

systemConstants :: [(Identifier, Exp)]
systemConstants =
  [ ("__CONNECT", Const_Exp 100) ]

letBindingSystemConstants :: Exp -> Exp
letBindingSystemConstants body =
  foldr (\(name, val) acc -> Let_Exp name val acc)
        body
        systemConstants