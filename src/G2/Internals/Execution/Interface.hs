-- | Interface
--   Module for interacting and interfacing with the symbolic execution engine.
module G2.Internals.Execution.Interface
    ( runNBreadth
    , runNBreadthHist
    , runNDepth
    , runNDepthHist
    , runNDepthHist'
    , runNDepthCatchError) where

import G2.Internals.Execution.Rules
import G2.Internals.Language.Support

runNBreadth :: [State] -> Int -> [State]
runNBreadth [] _ = []
runNBreadth states 0 = states
runNBreadth states n = runNBreadth (concatMap (snd . reduce) states) (n - 1)

runNBreadthHist :: [([Rule], State)] -> Int -> [([Rule], State)]
runNBreadthHist [] _ = []
runNBreadthHist rss 0 = rss
runNBreadthHist rss n = runNBreadthHist (concatMap go rss) (n - 1)
  where
    go :: ([Rule], State) -> [([Rule], State)]
    go (rules, state) = let (rule, states) = reduce state
                        in map (\s -> (rules ++ [rule], s)) states

runNDepth :: [State] -> Int -> [State]
runNDepth states depth = runNDepth' (map (\s' -> (s', depth)) states)
    where
        runNDepth' :: [(State, Int)] -> [State]
        runNDepth' [] = []
        runNDepth' ((s, 0):xs) = s:runNDepth' xs
        runNDepth' ((s, n):xs) =
            let
                (_, red) = reduce $ s
                s'' = map (\s' -> (s', n - 1)) red
            in
            runNDepth' (s'' ++ xs)

runNDepthHist :: [State] -> Int -> [[(Maybe Rule, State)]]
runNDepthHist states d = runNDepth' (map (\s' -> ([(Nothing, s')], d)) states)
    where
        runNDepth' :: [([(Maybe Rule, State)], Int)] -> [[(Maybe Rule, State)]]
        runNDepth' [] = []
        runNDepth' ((rss, 0):xs) = rss:runNDepth' xs
        runNDepth' ((rss@((_, s):_), n):xs) =
            let
                (r', red) = reduce $ s
                s'' = map (\s' -> ((Just r', s'):rss, n - 1)) red
            in
            runNDepth' (s'' ++ xs)

runNDepthHist' :: [State] -> Int -> [([Rule], State)]
runNDepthHist' states d = runNDepth' $ map (\s -> (([], s), d)) states
  where
    runNDepth' :: [(([Rule], State), Int)] -> [([Rule], State)]
    runNDepth' [] = []
    runNDepth' ((rss, 0):xs) = rss : runNDepth' xs
    runNDepth' ((((rs, s), n)):xs) =
        let (app_rule, reduceds) = reduce s
            mod_info = map (\s' -> ((rs ++ [app_rule], s'), n - 1)) reduceds
        in runNDepth' (mod_info ++ xs)

runNDepthCatchError :: [State] -> Int -> Either [State] State
runNDepthCatchError states d = runNDepth' (map (\s' -> (s', d)) states)
    where
        runNDepth' :: [(State, Int)] -> Either [State] State
        runNDepth' [] = Left []
        runNDepth' ((s, 0):xs) =
            case runNDepth' xs of
                Left xs' -> Left (s:xs')
                Right x -> Right x
        runNDepth' ((s, n):xs) =
            let
                (r, red) = reduce $ s
                s'' = map (\s' -> (s', n - 1)) red
            in
            if r == RuleError then Right s else runNDepth' (s'' ++ xs)

{- TODO: What here do we need?

    ( initState
    , runNBreadth
    , runNDepth
    , histN
    ) where

import G2.Internals.Language
import G2.Internals.Execution.Engine

import qualified Data.Char as C
import Data.List
import qualified Data.Map as M

-- | Lambda Arguments
--   Strips away the lambda function's arguments.
lamArgs :: Expr -> [(Name, Type)]
lamArgs (Lam n e (TyFun t _)) = (n, t):lamArgs e
lamArgs _ = []

-- | Fresh Seeded Name
freshSeededName' :: Name -> State -> Name
freshSeededName' seed state = stripped_seed ++ show (max_confs_num + 1)
  where confs         = allNames state
        max_confs_num = maximum $ [0] ++ (map nameNum confs)
        stripped_seed = filter (not . C.isDigit) seed

        nameNum :: Name -> Int
        nameNum name = case filter C.isDigit name of
            [] -> 0
            xs -> read xs :: Int

-- | Fresh Seeded Name List
freshSeededNameList' :: [Name] -> State -> [Name]
freshSeededNameList' [] _     = []
freshSeededNameList' (n:ns) s = [n'] ++ freshSeededNameList' ns s'
  where n' = freshSeededName' n s
        s' = bindExpr n' BAD s  -- Conflict

-- | Fresh Argument Names
--   Gets fresh argument names based on the expression environment.
freshArgNames :: EEnv -> Name -> [(Name, Type)]
freshArgNames eenv entry = zip arg_names arg_types
  where entry_expr = case (lookupExpr entry eenv) of
            Just ex -> ex
            Nothing -> error "Entry not found"
        args = lamArgs entry_expr
        arg_names = map fst args
        arg_types = map snd args
        fresh_names = freshSeededNameList' arg_names fake_state
        fake_state  = State { expr_env     = eenv
                            , type_env     = M.empty
                            , curr_expr    = BAD
                            , path_cons    = []
                            , sym_links    = M.empty
                            , func_interps = M.empty
                            , all_names    = M.empty
                            , exec_stack   = Stack.empty
                            , cond_stack   = Stack.empty }

-- | Make Symbolic Links
--   Construct a the current expression and a symbolic link table given the
--   entry point name, should it exist in the environment.
mkSymLinks :: EEnv -> Name -> [(Name, Type)] -> (Expr, SymLinkTable)
mkSymLinks eenv entry args = (curr_expr, sym_links)
  where entry_expr = case (lookupExpr entry eenv) of
            Just ex -> ex
            Nothing -> error "Entry not found"
        entry_type = typeOf entry_expr
        arg_names  = map fst args
        arg_types  = map snd args
        slt_rhs    = zip3 arg_names arg_types (map Just [1..])
        sym_links  = M.fromList (zip arg_names slt_rhs)
        curr_expr  = foldl (\acc (n, t) -> App acc (Var n t))
                           (Var entry entry_type)
                           args

-- | Flatten Type
--   Flattens a Type. For instance:
--       a -> b -> c  flattens to  [a, b, c]
flattenType :: Type -> [Type]
flattenType (TyFun tf ta) = tf : flattenType ta
flattenType _ = []

-- | Initialize State with Assume / Assert Conditions
initState :: TEnv -> EEnv -> Maybe Name -> Maybe Name -> Name -> State
initState tenv eenv m_assume m_assert entry =
  case M.lookup entry eenv of
    Just entry_ex ->
      let args'    = freshArgNames eenv entry
          entry_ty = typeOf entry_ex
          (expr', slt) = mkSymLinks eenv entry args'

          (expr'', assume_ty) = addAssumeAssert Assume m_assume args' eenv expr'
          (expr''', assert_ty) = addAssumeAssert Assert m_assert args' eenv expr''
      in if ((flattenType entry_ty) == (init $ flattenType assume_ty) || m_assume == Nothing) &&
            ((flattenType entry_ty) == (init $ flattenType assert_ty) || m_assert == Nothing)
          then let pre_state = State { expr_env     = eenv
                                     , type_env     = tenv
                                     , curr_expr    = expr'''
                                     , path_cons    = []
                                     , sym_links    = slt
                                     , func_interps = M.empty
                                     , all_names    = M.empty
                                     , exec_stack   = Stack.empty
                                     , cond_stack   = Stack.empty }
                   all_names = allNamesMap pre_state
               in pre_state {all_names = all_names}
          else error "Type(s) mismatch for Assume or Assert\n"
    otherwise -> error $ "No matching entry points for " ++ entry
    where
        addAssumeAssert :: (Expr -> Expr -> Expr) -> Maybe Name -> [(Name, Type)] -> EEnv -> Expr -> (Expr, Type)
        addAssumeAssert _ Nothing _ _ e = (e, TyFun TyBottom TyBottom)
        addAssumeAssert f (Just a) args eenv e =
            case M.lookup a eenv of
                Nothing -> error "Could not find function"
                Just a_ex -> (f (fst $ mkSymLinks eenv a args) e, typeOf a_ex)

-- | Run n Times
--   Run a state n times through the power of concatMap.
runNBreadth :: [State] -> Int -> [State]
runNBreadth states 0 = states
runNBreadth [] n     = []
runNBreadth states n = runNBreadth (concatMap (\s -> step s) states) (n - 1)

runNDepth :: [State] -> Int -> [State]
runNDepth s n = runNDepth' (map (\s' -> (s', n)) s)
    where
        runNDepth' :: [(State, Int)] -> [State]
        runNDepth' [] = []
        runNDepth' ((s, 0):xs) = s:runNDepth' xs
        runNDepth' ((s, n):xs) =
            let
                s'' = map (\s' -> (s', n - 1)) (step s)
            in
            runNDepth' (s'' ++ xs)

-- | History n Times
--   Run a state n times, while keeping track of its history as a list.
histN :: [State] -> Int -> [([State], Int)]
histN states 0 = [(states, 0)]
histN [] n     = [([], n - 1)]
histN states n = (states', n):(histN states' (n - 1))
  where states' = concatMap step states
-}

