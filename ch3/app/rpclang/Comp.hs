{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
module Comp where 

import qualified Expr as R 
import qualified Actors.Expr as A  

import qualified Data.Map as Map
import qualified Data.Set as Set

type Identifier = String
type Location = String
type SEnv = Map.Map Identifier Identifier 
type Cont = A.Exp -> Integer -> Table -> (A.Exp, Integer, Table)
type Table = Map.Map Integer (Location, [Identifier], Identifier, A.Exp)  -- (y1,...,yk, x, e)

constCREATECLO, constCALLCLO :: String
constCREATECLO = "CREATECLO"
constCALLCLO   = "CALLCLO"

comp :: R.Exp -> Location -> SEnv -> Integer -> Table -> Cont -> (A.Exp, Integer, Table)
comp (R.Var_Exp x) loc senv n tbl k = 
    let y = apply_senv senv x in
        k (A.Var_Exp y) n tbl
    
comp (R.Proc_Exp locb x e) loc senv n tbl k =
    let fnum = n
        fName = "f" ++ show fnum
        fvs1 = Set.toList (Set.delete x (R.fv e) `Set.union` Set.singleton locb)
        fvs = map (apply_senv senv) fvs1
        fvsName = "fvs" ++ show (n+1)
        x1 = x ++ show (n+2)
        cloName = "clo" ++ show (n+3)
        (body, n1, tbl1) = comp e locb (Map.insert x x1 senv) (n+4) tbl (,,) 
        fclosed = mkProc fvsName fvs x1 body
        tbl2 = Map.insert fnum (locb, fvs, x1, fclosed) tbl1
    in
        if loc == locb then
            k ( A.Tuple_Exp 
                    [ A.Var_Exp fName,
                      A.Tuple_Exp (map A.Var_Exp fvs) ] ) n1 tbl2
        else
            let (contExp, n2, tbl3) = k (A.Var_Exp cloName) n1 tbl2 in
                (A.Block_Exp [
                    A.Send_Exp [
                        A.Var_Exp locb, A.Var_Exp constCREATECLO, 
                            A.Tuple_Exp [
                                A.Const_Exp (fromInteger fnum),
                                A.Tuple_Exp (map A.Var_Exp fvs),
                                A.Var_Exp loc
                            ]
                    ],
                    A.Ready_Exp (A.Proc_Exp cloName contExp) ], n2, tbl3)
    where
        mkProc :: Identifier -> [Identifier] -> Identifier -> A.Exp -> A.Exp
        mkProc fvsName ys x body = 
            A.Proc_Exp fvsName 
                (A.LetTuple_Exp ys (A.Var_Exp fvsName) 
                    (A.Proc_Exp x body))
    
comp (R.Call_Exp e1 locb e2) loc senv n tbl k =
    comp e1 loc senv n tbl (\clo n1 tbl1 -> 
        comp e2 loc senv n1 tbl1 (\arg n2 tbl2 ->
            if loc == locb then
                k (A.Call_Exp clo arg) n2 tbl2
            else
                let retName = "ret" ++ show n2 
                    (contExp, n3, tbl3) = k (A.Var_Exp retName) (n2+1) tbl2
                in 
                (A.Block_Exp [
                    A.Send_Exp [
                        A.Var_Exp locb, 
                        A.Var_Exp constCALLCLO, 
                        A.Tuple_Exp [
                            clo,
                            arg,
                            A.Var_Exp loc
                        ]
                    ],
                    A.Ready_Exp (A.Proc_Exp retName contExp) ], n3, tbl3) ))
    
comp (R.Const_Exp i) loc senv n tbl k =
    k (A.Const_Exp i) n tbl

comp (R.Diff_Exp e1 e2) loc senv n tbl k =
    comp e1 loc senv n tbl (\e1' n1 tbl1 ->
        comp e2 loc senv n1 tbl1 (\e2' n2 tbl2 ->
            k (A.Diff_Exp e1' e2') n2 tbl2))

comp (R.IsZero_Exp e) loc senv n tbl k =
    comp e loc senv n tbl (\e1' n1 tbl1 ->
        k (A.Unary_Exp A.IsZero e1') n1 tbl1)

comp (R.If_Exp e1 e2 e3) loc senv n tbl k =
    comp e1 loc senv n tbl (\e1' n1 tbl1 ->
        comp e2 loc senv n1 tbl1 (\e2' n2 tbl2 ->
            comp e3 loc senv n2 tbl2 (\e3' n3 tbl3 ->
                k (A.If_Exp e1' e2' e3') n3 tbl3)))

comp (R.Let_Exp x e1 e2) loc senv n tbl k =
    comp e1 loc senv n tbl (\e1' n1 tbl1 ->
        let y = x ++ show n1
            senv1 = Map.insert x y senv 
            (e2', n2, tbl2) = comp e2 loc senv1 (n1+1) tbl1 k
        in (A.Let_Exp y e1' e2', n2, tbl2))

compMain :: R.Exp -> A.Exp
compMain e = 
    let 
        locSet = fvLoc e
        senv = Map.fromList [(loc, loc) | loc <- Set.toList locSet]
        n = 0
        tbl = Map.empty
        (e1,n1,tbl1) = comp e "main" senv n tbl (,,) -- Wow! (,,) = \e n tbl -> (e, n, tbl)) 
    in actorTemplate "main" tbl1 
        (createActors tbl1 e1)

actorTemplate :: String -> Table -> A.Exp -> A.Exp
actorTemplate actorName tbl e = 
    let funDeclList = Map.toList tbl in 
    A.Proc_Exp actorName
        (A.Let_Exp constCREATECLO (A.Const_Exp 1)
        (A.Let_Exp constCALLCLO (A.Const_Exp 2)
        (funDecls funDeclList 
        (dispatchDecl (map fst funDeclList) 
        (threeDriverDecl e))) ))

funDecls :: [(Integer, (Location, [A.Identifier], A.Identifier, A.Exp))] -> A.Exp -> A.Exp
funDecls [] e = e
funDecls ((fnum, (loc, fvs, x, p)):funDeclList) e = 
    A.Let_Exp ("F" ++ show fnum) (A.Const_Exp (fromInteger fnum))
    (A.Let_Exp ("f" ++ show fnum) p
        (funDecls funDeclList e))

dispatchDecl :: [Integer] -> A.Exp -> A.Exp
dispatchDecl fnumList e =
    A.Let_Exp "dispatch"
        (A.Proc_Exp "fNO"
            (dispatchDecl' fnumList)) e

dispatchDecl' :: [Integer] -> A.Exp
dispatchDecl' [] = A.Const_Exp 0  -- 0 will cause a runtime error on invocation!
dispatchDecl' (fnum:fnumList) = 
    A.If_Exp 
        (A.Unary_Exp A.IsZero 
            (A.Diff_Exp (A.Var_Exp "fNO") 
            (A.Const_Exp (fromInteger fnum))))
        (A.Var_Exp ("f" ++ show fnum))
        (dispatchDecl' fnumList)

threeDriverDecl :: A.Exp -> A.Exp
threeDriverDecl e =
    A.Letrec_Exp 
        [
            mainDriverDecl,
            createCloDecl,
            callCloDecl
        ] e
        
mainDriverDecl :: (Identifier, Identifier, A.Exp)
mainDriverDecl =
    ("mainLoop", "msg", 
        A.If_Exp 
            (A.Unary_Exp A.IsZero 
                (A.Diff_Exp 
                    (A.Var_Exp "msg") 
                    (A.Var_Exp constCREATECLO)))
            (A.Ready_Exp (A.Var_Exp "createClo"))
        (A.If_Exp 
            (A.Unary_Exp A.IsZero 
                (A.Diff_Exp 
                    (A.Var_Exp "msg") 
                    (A.Var_Exp constCALLCLO)))
            (A.Ready_Exp (A.Var_Exp "callClo"))
            (A.Ready_Exp (A.Var_Exp "mainLoop"))))

createCloDecl :: (Identifier, Identifier, A.Exp)
createCloDecl = 
    ("createClo", "msg", 
        A.LetTuple_Exp ["fNO", "fvs", "sender"] (A.Var_Exp "msg")
        (A.Let_Exp "f" (A.Call_Exp (A.Var_Exp "dispatch") (A.Var_Exp "fNO"))
        (A.Let_Exp "clo" (A.Tuple_Exp [A.Var_Exp "f", A.Var_Exp "fvs"])
        (A.Block_Exp 
            [
                A.Send_Exp [A.Var_Exp "sender", A.Var_Exp "clo"],
                A.Ready_Exp (A.Var_Exp "mainLoop")
            ]))))

callCloDecl :: (Identifier, Identifier, A.Exp)
callCloDecl = 
    ("callClo", "msg", 
        A.LetTuple_Exp ["clo", "arg", "sender"] (A.Var_Exp "msg")
        (A.LetTuple_Exp ["f", "fvs"] (A.Var_Exp "clo")
        (A.Let_Exp "ffvs" (A.Call_Exp (A.Var_Exp "f") (A.Var_Exp "fvs"))
        (A.Let_Exp "ret" (A.Call_Exp (A.Var_Exp "ffvs") (A.Var_Exp "arg"))
        (A.Block_Exp 
            [
                A.Send_Exp [A.Var_Exp "sender", A.Var_Exp "ret"],
                A.Ready_Exp (A.Var_Exp "mainLoop")
            ])))))

createActors :: Table -> A.Exp -> A.Exp
createActors tbl e = 
    let locList = 
            Set.toList 
                (Set.fromList 
                    [loc | (_, (loc,_,_,_)) <- Map.toList tbl]) in 
        createActors' locList tbl e

createActors' :: [Location] -> Table -> A.Exp -> A.Exp
createActors' [] tbl e = e
createActors' (loc:locList) tbl e = 
    let e1 = createActors' locList tbl e 
        p = actorTemplate "self" tbl (A.Ready_Exp (A.Var_Exp "mainLoop"))
        bName = "behaviour" ++ loc
        aName = loc
    in A.Let_Exp bName p 
        (A.Let_Exp aName (A.New_Exp (A.Var_Exp bName)) e1)
    
--
apply_senv :: SEnv -> Identifier -> Identifier
apply_senv senv x = case Map.lookup x senv of
    Just y  -> y
    Nothing -> error ("unbound variable: " ++ x)

--
fvLoc :: R.Exp -> Set.Set Location
fvLoc (R.Const_Exp _) = Set.empty
fvLoc (R.Diff_Exp e1 e2) = fvLoc e1 `Set.union` fvLoc e2
fvLoc (R.IsZero_Exp e) = fvLoc e
fvLoc (R.If_Exp e1 e2 e3) = fvLoc e1 `Set.union` fvLoc e2 `Set.union` fvLoc e3
fvLoc (R.Var_Exp _) = Set.empty
fvLoc (R.Let_Exp _ e1 e2) = fvLoc e1 `Set.union` fvLoc e2
fvLoc (R.Proc_Exp loc _ e) = Set.singleton loc `Set.union` fvLoc e
fvLoc (R.Call_Exp e1 _ e2) = fvLoc e1 `Set.union` fvLoc e2
