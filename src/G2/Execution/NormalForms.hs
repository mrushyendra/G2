module G2.Execution.NormalForms where

import G2.Language
import qualified G2.Language.Stack as S
import qualified G2.Language.ExprEnv as E

import qualified Data.List as L

-- | If something is in "value form", then it is essentially ready to be
-- returned and popped off the heap. This will be the SSTG equivalent of having
-- Return vs Evaluate for the ExecCode of the `State`.
--
-- So in this context, the following are considered NOT-value forms:
--   `Var`, only if a lookup still available in the expression environment.
--   `App`, which involves pushing the RHS onto the `Stack`, if the center is not a Prim or DataCon
--   `Let`, which involves binding the binds into the eenv
--   `Case`, which involves pattern decomposition and stuff.
isExprValueForm :: E.ExprEnv -> Expr -> Bool
isExprValueForm eenv (Var var) =
    E.lookup (idName var) eenv == Nothing || E.isSymbolic (idName var) eenv
isExprValueForm eenv (App f a) = case unApp (App f a) of
    (Prim _ _:xs) -> all (isExprValueForm eenv) xs
    (Data _:_) -> True
    ((Var _):_) -> False
    _ -> False
isExprValueForm _ (Let _ _) = False
isExprValueForm _ (Case _ _ _) = False
isExprValueForm eenv (Cast e (t :~ _)) = not (hasFuncType t) && isExprValueForm eenv e
isExprValueForm _ (Tick _ _) = False
isExprValueForm _ (NonDet _) = False
isExprValueForm _ (SymGen _) = False
isExprValueForm _ (Assume _ _ _) = False
isExprValueForm _ (Assert _ _ _) = False
isExprValueForm _ _ = True

-- | Is the execution state in a value form of some sort? This would entail:
-- * The `Stack` is empty.
-- * The `ExecCode` is in a `Return` form.
-- * We have no path conds to reduce
isExecValueForm :: State t -> Bool
isExecValueForm state | Nothing <- S.pop (exec_stack state)
                      , CurrExpr Return _ <- curr_expr state
                      , non_red_path_conds state == [] = True
                      | otherwise = False


isExecValueFormDisNonRedPC :: State t -> Bool
isExecValueFormDisNonRedPC s = isExecValueForm $ s {non_red_path_conds = []}

-- Expr is in Symbolic Merged Normal Form if it is in SWHNF, or if it is a Case Expr on a LitInt and all the Alt Exprs
-- are unique concrete Data Constructors
isSMNF :: E.ExprEnv -> Expr -> Bool
isSMNF _ (Case (Var (Id _ t)) _ a)
    | TyLitInt <- t
    , all isLitAlt a
    , aexprTypes <- map (\(Alt (LitAlt _) aexpr) -> typeOf aexpr) a
    , all isADT aexprTypes
    , (length . L.nub $ map (\ty -> tyAppCenter ty) aexprTypes) == 1 = True
isSMNF eenv e = isExprValueForm eenv e

isLitAlt :: Alt -> Bool
isLitAlt (Alt (LitAlt _) _) = True
isLitAlt _ = False
