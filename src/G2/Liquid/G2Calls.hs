{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module G2.Liquid.G2Calls ( checkAbstracted
                         , reduceCalls
                         , mapAccumM) where

import G2.Config
import G2.Execution
import G2.Interface
import G2.Language as G2
import qualified G2.Language.ExprEnv as E
import G2.Liquid.Helpers
import G2.Liquid.LHReducers
import G2.Liquid.Types
import G2.Solver

import G2.Lib.Printers

import Control.Monad
import qualified Data.HashMap.Lazy as HM
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Data.Monoid

-------------------------------
-- Check Abstracted
-------------------------------
-- Checks if the abstracted functions actually deviate from the real function behavior.
-- If they do not, they can simply be eliminated from the state.

-- The result of a call to checkAbstracted'.  Either:
-- (1) the function does need to be abstract, and we get the actual result of executing the function call. 
-- (2) the function does not need to be abstract, but its assertion is violated
-- (3) the function does not need to be abstract
data AbstractedRes = AbstractRes Abstracted Model
                   | NotAbstractResButViolated FuncCall Model
                   | NotAbstractRes

toAbstracted :: AbstractedRes -> Maybe Abstracted
toAbstracted (AbstractRes a _) = Just a
toAbstracted _ = Nothing

toModel :: AbstractedRes -> Maybe Model
toModel (AbstractRes _ m) = Just m
toModel (NotAbstractResButViolated _ m) = Just m
toModel _ = Nothing

-- | Checks if abstracted functions actually had to be abstracted.
checkAbstracted :: (Solver solver, Simplifier simplifier) => solver -> simplifier -> Config -> Bindings -> ExecRes LHTracker -> IO (ExecRes [Abstracted])
checkAbstracted solver simplifier config bindings er@(ExecRes{ final_state = s@State { track = lht }}) = do
    let check = checkAbstracted' solver simplifier (sharing config) s bindings
    abstractedR <- mapM check (abstract_calls lht)
    let abstracted' = mapMaybe toAbstracted $ abstractedR
        models = mapMaybe toModel $ abstractedR

    return $ er { final_state = s {track = abstracted', model = foldr HM.union (model s) models }}

checkAbstracted' :: (Solver solver, Simplifier simplifier)
                 => solver
                 -> simplifier
                 -> Sharing
                 -> State LHTracker
                 -> Bindings
                 -> FuncCall
                 -> IO AbstractedRes
checkAbstracted' solver simplifier share s bindings abs_fc@(FuncCall { funcName = n, arguments = ars, returns = r })
    | Just e <- E.lookup n $ expr_env s = do
        let 
            e' = mkApp $ Var (Id n (typeOf e)):ars

            ds = deepseq_walkers bindings
            strict_call = maybe e' (fillLHDictArgs ds) $ mkStrict_maybe ds e'

        -- We leave assertions in the code, and set true_assert to false so we can
        -- tell if an assertion was violated.
        -- If an assertion is violated, it means that the function did not need to be abstracted,
        -- but does need to be in `assert_ids` .
        let s' = pickHead . modelToExprEnv $
                   s { curr_expr = CurrExpr Evaluate strict_call }

        (er, _) <- runG2WithSomes 
                        (SomeReducer (StdRed share solver simplifier))
                        (SomeHalter SWHNFHalter)
                        (SomeOrderer NextOrderer)
                        solver simplifier emptyMemConfig s' bindings

        case er of
            [ExecRes
                {
                    final_state = (State { curr_expr = CurrExpr _ ce, model = m})
                }] -> case not $ ce `eqUpToTypes` r of
                        True ->
                            return $ AbstractRes 
                                        ( Abstracted { abstract = abs_fc
                                                     , real = abs_fc { returns = ce } }
                                        ) m
                        False -> return NotAbstractRes
            _ -> error $ "checkAbstracted': Bad return from runG2WithSomes"
    | otherwise = error $ "checkAbstracted': Bad lookup in runG2WithSomes"

-------------------------------
-- Reduce Calls
-------------------------------
-- Reduces the arguments and results of the violated and abstracted functions to normal form.

reduceCalls :: (Solver solver, Simplifier simplifier) => solver -> simplifier -> Config -> Bindings -> ExecRes LHTracker -> IO (Bindings, ExecRes LHTracker)
reduceCalls solver simplifier config bindings er = do
    (bindings', er') <- reduceViolated solver simplifier (sharing config) bindings er
    (bindings'', er'') <- reduceAbstracted solver simplifier (sharing config) bindings' er'

    return (bindings'', er'')

reduceViolated :: (Solver solver, Simplifier simplifier) => solver -> simplifier -> Sharing -> Bindings -> ExecRes LHTracker -> IO (Bindings, ExecRes LHTracker)
reduceViolated solver simplifier share bindings er@(ExecRes { final_state = s, violated = Just v }) = do
    let red = SomeReducer (StdRed share solver simplifier)
    (bindings', v') <- reduceFuncCall share red solver simplifier s bindings v
    -- putStrLn $ "v = " ++ show v
    -- putStrLn $ "v' = " ++ show v'
    return (bindings', er { violated = Just v' })
reduceViolated _ _ _ b er = return (b, er) 

reduceAbstracted :: (Solver solver, Simplifier simplifier) => solver -> simplifier -> Sharing -> Bindings -> ExecRes LHTracker -> IO (Bindings, ExecRes LHTracker)
reduceAbstracted solver simplifier share bindings
                er@(ExecRes { final_state = (s@State { track = lht}) }) = do
    let red = SomeReducer (StdRed share solver simplifier)
        fcs = abstract_calls lht

    (bindings', fcs') <- mapAccumM (reduceFuncCall share red solver simplifier s) bindings fcs

    return (bindings', er { final_state = s { track = lht { abstract_calls = fcs' } }})

reduceFuncCall :: (Solver solver, Simplifier simp) => Sharing -> SomeReducer LHTracker -> solver -> simp -> State LHTracker -> Bindings -> FuncCall -> IO (Bindings, FuncCall)
reduceFuncCall share red solver simplifier s bindings fc@(FuncCall { arguments = ars, returns = r }) = do
    -- (bindings', red_ars) <- mapAccumM (reduceFCExpr share (red <~ SomeReducer (Logger "arg")) solver simplifier s) bindings ars
    -- (bindings'', red_r) <- reduceFCExpr share (red <~ SomeReducer (Logger "ret")) solver simplifier s bindings' r
    (bindings', red_ars) <- mapAccumM (reduceFCExpr share red solver simplifier s) bindings ars
    (bindings'', red_r) <- reduceFCExpr share red solver simplifier s bindings' r

    return (bindings'', fc { arguments = red_ars, returns = red_r })

reduceFCExpr :: (Solver solver, Simplifier simp) => Sharing -> SomeReducer LHTracker -> solver -> simp -> State LHTracker -> Bindings -> Expr -> IO (Bindings, Expr)
reduceFCExpr share reducer solver simplifier s bindings e 
    | not . isTypeClass (type_classes s) $ (typeOf e)
    , ds <- deepseq_walkers bindings
    , Just strict_e <-  mkStrict_maybe ds e  = do
        let 
            e' = fillLHDictArgs ds strict_e

        let s' = elimAsserts . pickHead . modelToExprEnv $
                   s { curr_expr = CurrExpr Evaluate e'}

        (er, bindings') <- runG2WithSomes 
                    reducer
                    (SomeHalter SWHNFHalter)
                    (SomeOrderer NextOrderer)
                    solver simplifier emptyMemConfig s' bindings

        case er of
            [er'] -> do
                let (CurrExpr _ ce) = curr_expr . final_state $ er'
                return (bindings { name_gen = name_gen bindings' }, ce)
            _ -> error "reduceAbstracted: Bad reduction"
    | otherwise = return (bindings, e) 

mapAccumM :: (Monad m, MonadPlus p) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, p y)
mapAccumM _ z [] = return (z, mzero)
mapAccumM f z (x:xs) = do
  (z', y) <- f z x
  (z'', ys) <- mapAccumM f z' xs
  return (z'', return y `mplus` ys)

modelToExprEnv :: State t -> State t
modelToExprEnv s = s { expr_env = model s `E.union'` expr_env s
                     , model = HM.empty }

-------------------------------
-- Generic
-------------------------------

pickHead :: (ASTContainer m Expr) => m -> m
pickHead = modifyASTs pickHead'

pickHead' :: Expr -> Expr
pickHead' (NonDet xs) = head xs
pickHead' e = e