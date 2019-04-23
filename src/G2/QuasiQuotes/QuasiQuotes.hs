{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module G2.QuasiQuotes.QuasiQuotes (g2) where

import G2.Config
import G2.Execution.Reducer
import G2.Initialization.MkCurrExpr
import qualified G2.Language.ExprEnv as E
import G2.Interface
import G2.Language as G2
import qualified G2.Language.Typing as Ty
import G2.Solver
import G2.Translation.Interface
import G2.Translation.TransTypes
import G2.QuasiQuotes.FloodConsts
import G2.QuasiQuotes.G2Rep

import Control.Monad

import Data.Data
import Data.List
import Data.Maybe
import qualified Data.Text as T

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote

import System.IO
import System.IO.Temp

import System.FilePath

g2 :: QuasiQuoter
g2 = QuasiQuoter { quoteExp = parseHaskellQ
                 , quotePat = error "g2: No QuasiQuoter for patterns."
                 , quoteType = error "g2: No QuasiQuoter for types."
                 , quoteDec = error "g2: No QuasiQuoter for declarations." }

parseHaskellQ :: String -> Q Exp
parseHaskellQ str = do
    (xs, b) <- parseHaskellQ' str

    -- let CurrExpr _ ce = curr_expr $ head s

    -- exp <- dataToExpQ (\a -> liftText <$> cast a) ce

    let xs' = addRegVarPasses str xs b
    solveStates xs' b


liftDataT :: Data a => a -> Q Exp
liftDataT = dataToExpQ (\a -> liftText <$> cast a)
    where
        liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

parseHaskellQ' :: String -> Q ([State ()], Bindings)
parseHaskellQ' s = do
    ms <- reifyModule =<< thisModule
    runIO $ do
        print ms
        parseHaskellIO s

-- | Turn the Haskell into a G2 Expr.  All variables- both those that the user
-- marked to be passed into the Expr as real values, and those that the user
-- wants to solve for- are treated as symbolic here.
parseHaskellIO :: String -> IO ([State ()], Bindings)
parseHaskellIO str = do
    print $ grabRegVars str
    print $ grabSymbVars str
    (_, exG2) <- withSystemTempFile "ThTemp.hs"
            (\filepath handle -> do
                hPutStrLn handle $ "module ThTemp where\ng2Expr = " ++ subSymb str
                hFlush handle
                hClose handle
                translateLoaded (takeDirectory filepath) filepath []
                    simplTranslationConfig mkConfigDef)
  
    let (s, is, b) = initState' exG2 "g2Expr" (Just "ThTemp") (mkCurrExpr Nothing Nothing) mkConfigDef
    
    SomeSolver con <- initSolver mkConfigDef
    case initRedHaltOrd con mkConfigDef of
        (SomeReducer red, SomeHalter hal, SomeOrderer ord) -> do
            xsb@(xs, _) <- runG2ThroughExecution red hal ord [] s b

            mapM_ (\st -> do
                print . curr_expr $ st
                print . path_conds $ st) xs

            return xsb

-- | Adds the appropriate number of lambda bindings to the Exp,
-- and sets up a conversion from TH Exp's to G2 Expr's.
-- The returned Exp should have type (State t).
addRegVarPasses :: Data t => String -> [State t] -> Bindings -> Q Exp
addRegVarPasses str xs@(s:_) (Bindings { input_names = is }) = do
    let regs = grabRegVars str

    runIO $ putStrLn "HERE"

    -- Get names for the lambdas for the regular inputs
    ns <- mapM newName regs
    let ns_pat = map varP ns
        ns_exp = map varE ns

    let is_exp = liftDataT is

        xs_exp = liftDataT xs
        s_exp = liftDataT s

        eenv_exp = appE (varE 'expr_env) s_exp
        tenv_exp = appE (varE 'type_env) s_exp

        g2Rep_exp = appE (varE 'g2Rep) tenv_exp
        ns_expr = map (appE g2Rep_exp . varE) ns

        zip_exp = appE (appE (varE 'zip) is_exp) $ listE ns_expr
        flooded_exp = appE (varE 'mapMaybe) (appE (varE 'floodConstants) zip_exp)

        flooded_states = appE flooded_exp xs_exp

        lam_binds = foldr (\n -> lamE [n]) flooded_states ns_pat

    lam_binds
addRegVarPasses _ _ _ = error "QuasiQuoter: No valid solutions found"

-- Takes an Exp representing a list of States, and returns an Exp
-- representing an ExecRes
solveStates :: Q Exp -> Bindings -> Q Exp
solveStates xs b = varE 'solveStates' `appE` liftDataT b `appE` xs

solveStates' :: Bindings -> [State t] -> ExecRes t
solveStates' = undefined

grabRegVars :: String -> [String]
grabRegVars s =
    let
        s' = dropWhile (== ' ') s
    in
    case s' of
        '\\':xs -> grabVars "->" xs
        _ -> error "Bad QuasiQuote"

afterRegVars :: String -> String
afterRegVars s = strip s
    where 
        strip ('-':'>':xs) = xs
        strip (x:xs) = strip xs
        strip [] = []

grabSymbVars :: String -> [String]
grabSymbVars s =
    let
        s' = dropWhile (== ' ') $ afterRegVars s
    in
    case s' of
        '\\':xs -> grabVars "?" xs
        _ -> error "Bad QuasiQuote"

grabVars :: String -> String -> [String]
grabVars _ [] = []
grabVars en (' ':xs) = grabVars en xs
grabVars en xs
    |  take (length en) xs == en = []
grabVars en xs@(_:_) =
    let
        (x, xs') = span (/= ' ') xs
    in
    x:grabVars en xs'

-- | Replaces the first '?' with '->'
subSymb :: String -> String
subSymb = sub
    where
        sub ('?':xs) = "->" ++ xs
        sub (x:xs) = x:sub xs
        sub "" = ""

-- liftDataToExpr :: Data a => ExprEnv -> TypeEnv ->  a -> Q Expr
-- liftDataToExpr eenv tenv = dataToExpr eenv tenv (const Nothing)

expToExprQ :: ExprEnv -> TypeEnv -> Q Exp -> Q Expr
expToExprQ eenv tenv expq = do
    ex <- expq
    return $ expToExpr eenv tenv ex

-- Modeled after dataToExpQ
expToExpr :: ExprEnv -> TypeEnv -> Exp -> Expr
expToExpr _ tenv (ConE n)
    | n' <- thNameToName (names tenv) n = Data (DataCon n' undefined)
expToExpr _ _ (LitE l) = Lit $ litToG2Lit l
expToExpr eenv tenv (AppE e1 e2) = App (expToExpr eenv tenv e1) (expToExpr eenv tenv e2)
expToExpr _ _ e = error $ "expToExpr: Unhandled case.\n" ++ show e

-- dataToExpr :: Data a => ExprEnv -> TypeEnv -> (forall b . Data b => b -> Maybe (Q Expr)) -> a -> Q Expr
-- dataToExpr eenv tenv = dataToQa vOrCE lE (foldl apE)
--     where
--         vOrCE s =
--             case nameSpace s of
--                 Just VarName
--                     | n <- thNameToName (E.keys eenv) s
--                     , Just t <- fmap Ty.typeOf $ E.lookup n eenv -> return (Var (Id n t))
--                 Just DataName
--                     | n <- thNameToName (names tenv) s -> return (Data undefined)
--                 _ -> error "Can't construct Expr from name"

--         apE x y = do
--             x' <- x
--             y' <- y
--             return (App x' y')
        
--         lE c = return (Lit $ litToG2Lit c)

thNameToName :: [G2.Name] -> TH.Name -> G2.Name
thNameToName ns thn =
    let
        (occ, mn) = thNameToOccMod thn
    in
    case find (\(G2.Name n mn' _ _) -> n == occ && mn == mn') ns of
        Just g2n -> g2n
        Nothing -> error "thNameToName: Can't find name"

thNameToOccMod :: TH.Name -> (T.Text, Maybe T.Text)
thNameToOccMod (TH.Name (OccName n) (NameG _ _ (ModName mn))) = (T.pack n, Just $ T.pack mn)
thNameToOccMod (TH.Name (OccName n) _) = (T.pack n, Nothing) 

litToG2Lit :: TH.Lit -> G2.Lit
litToG2Lit (IntPrimL i) = LitInt i 
litToG2Lit _ = error "litToG2Lit: Unsupported Lit"