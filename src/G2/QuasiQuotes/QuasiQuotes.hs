{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module G2.QuasiQuotes.QuasiQuotes ( g2
                                  , g2M
                                  , parseHaskellQ) where

import G2.Config
import G2.Execution.Interface
import G2.Execution.Memory
import G2.Execution.Reducer
import G2.Initialization.MkCurrExpr
import G2.Interface
import G2.Language as G2
import qualified G2.Language.Typing as Ty
import G2.Solver
import G2.Translation.Cabal.Cabal
import G2.Translation.Haskell
import G2.Translation.Interface
import G2.Translation.TransTypes
import G2.QuasiQuotes.FloodConsts
import G2.QuasiQuotes.G2Rep
import G2.QuasiQuotes.Support
import G2.QuasiQuotes.Parser

import qualified Control.Concurrent.Lock as Lock

import Data.Data
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import qualified G2.Language.ExprEnv as E

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote

import System.Directory
import System.IO
import System.IO.Temp
import System.IO.Unsafe

g2 :: QuasiQuoter
g2 = QuasiQuoter { quoteExp = parseHaskellQ
                 , quotePat = error "g2: No QuasiQuoter for patterns."
                 , quoteType = error "g2: No QuasiQuoter for types."
                 , quoteDec = error "g2: No QuasiQuoter for declarations." }

g2M :: QuasiQuoter
g2M = QuasiQuoter { quoteExp = parseHaskellMergeQ
                  , quotePat = error "g2: No QuasiQuoter for patterns."
                  , quoteType = error "g2: No QuasiQuoter for types."
                  , quoteDec = error "g2: No QuasiQuoter for declarations." }

-- If we compile multiple G2 quasiquoters at the same time, we can get errors.
-- This is a hack to prevent that from happening.  The IORef is global, and
-- aquired/released by each quasiquoter's compilation
oneByOne :: IORef Lock.Lock
oneByOne = unsafePerformIO $ newIORef =<< Lock.new

acquireIORefLock :: IO ()
acquireIORefLock = do
    lock <- readIORef oneByOne
    Lock.acquire lock

releaseIORefLock :: IO ()
releaseIORefLock = do
    lock <- readIORef oneByOne
    Lock.release lock

parseHaskellMergeQ :: String -> Q Exp
parseHaskellMergeQ str = do
    config <- runIO qqConfig
    parseHaskellConfigQ (config { stateMerging = Merging }) str

parseHaskellQ :: String -> Q Exp
parseHaskellQ str = do
    config <- runIO qqConfig
    parseHaskellConfigQ config str

parseHaskellConfigQ :: Config -> String -> Q Exp
parseHaskellConfigQ config str = do
    runIO $ acquireIORefLock
    -- runIO $ putStrLn $ "CWD is: " ++ cwd

    -- Get names for the lambdas for the regular inputs
    let qext = extractQuotedData str

    -- let regs = grabRegVars str
    let regs = map fst $ concVars qext

    ns <- mapM newName regs
    let ns_pat = map varP ns

    -- Get names for the lambdas for the regular inputs
    exG2 <- parseHaskellQ' qext
    let (init_s, _, init_b) = initState' exG2 (T.pack functionName) (Just $ T.pack moduleName)
                                        (mkCurrExpr Nothing Nothing) config

    runIO $ releaseIORefLock

    ex_out <- runExecutionQ (stateMerging config) init_s init_b config

    state_name <- newName "state"
    tenv_name <- newName "tenv_parse"
    cleaned_names_name <- newName "cleaned"
    bindings_name <- newName "bindings"

    (ex, state_exp, tenv, bindings_final) <- case ex_out of
        Completed xs b -> do
            case elimUnusedCompleted xs b of
                (xs'@(s:_), b') -> do
                    let xs'' = listE $ map (moveOutTypeEnvState tenv_name) xs'

                        xs''' = addCompRegVarPasses (varE state_name) tenv_name cleaned_names_name ns (inputIds s b') b'

                        b'' = b' { input_names = drop (length regs) (input_names b') }
                        sol = solveStates xs''' (varE bindings_name)
                        ars = extractArgs (inputIds s b'') (cleaned_names b'') tenv_name sol

                    return (foldr (\n -> lamE [n]) ars ns_pat, xs'', type_env s, b'')
                ([], _) ->
                    let
                        b' = init_b { input_names = drop (length regs) (input_names init_b) }
                        ts = map (toTHType (cleaned_names b') . Ty.typeOf) $ inputIds init_s b'
                        tup_t = case ts of
                                    [t] -> t
                                    _ -> foldr appT (tupleT (length ts)) ts

                    in
                    return (foldr (\n -> lamE [n]) [| return (Nothing :: Maybe $(tup_t)) |] ns_pat
                                  , [| return [] :: IO [State ()] |]
                                  , M.empty
                                  , b')
        NonCompleted s b -> do
            let 
                (s', b') = elimUnusedNonCompleted s b

                s'' = moveOutTypeEnvState tenv_name s'

                s''' = addedNonCompRegVarBinds (varE state_name) tenv_name cleaned_names_name ns (inputIds s' b') b'

                b'' = b' { input_names = drop (length regs) (input_names b') }

                sol = executeAndSolveStates (stateMerging config) s''' (varE bindings_name)

                ars = extractArgs (inputIds s b'') (cleaned_names b'') tenv_name sol

            return (foldr (\n -> lamE [n]) ars ns_pat, s'', type_env s', b'')

            -- foldr (\n -> lamE [n]) [|do putStrLn "NONCOMPLETED"; return Nothing;|] ns_pat


    let tenv_exp = liftDataT tenv `sigE` [t| TypeEnv |]
        bindings_exp = liftDataT (bindings_final { name_gen = mkNameGen ()})

    letE [ valD (varP state_name) (normalB state_exp) []
         , valD (varP tenv_name) (normalB tenv_exp) []
         , valD (varP bindings_name) (normalB bindings_exp) []
         , valD (varP cleaned_names_name) (normalB (varE 'cleaned_names `appE` varE bindings_name)) []] ex

liftDataT :: Data a => a -> Q Exp
liftDataT = dataToExpQ (\a -> case liftText <$> cast a of
                                    je@(Just _) -> je
                                    Nothing -> liftLoc <$> cast a)
    where
        liftText txt = appE (varE 'T.pack) (stringE (T.unpack txt))
        liftLoc (G2.Loc l c f) = conE 'G2.Loc `appE` intE l `appE` intE c `appE` stringE f
        intE i = [| i |]

parseHaskellQ' :: QuotedExtract -> Q ExtractedG2
parseHaskellQ' qext = do
  (ModuleInfo mods) <- reifyModule =<< thisModule
  -- runIO $ mapM putStrLn =<< guessModules mods
  -- runIO $ putStrLn "-----"
  runIO $ parseHaskellIO mods qext

-- | Turn the Haskell into a G2 Expr.  All variables- both those that the user
-- marked to be passed into the Expr as real values, and those that the user
-- wants to solve for- are treated as symbolic here.
parseHaskellIO :: [Module] -> QuotedExtract -> IO ExtractedG2
parseHaskellIO mods qext = do
    -- cwd <- getCurrentDirectory
    -- let cwd' = cwd ++ "/quasiquote/"
    let hskStr = quotedEx2Hsk qext
    (_, exG2) <- withSystemTempFile fileName
            (\filepath handle -> do
                let contents = "module " ++ moduleName ++ " where\n"
                                ++ intercalate "\n" modImports ++ "\n"
                                ++ functionName ++ " = " ++ hskStr
                hPutStrLn handle contents
                hFlush handle
                hClose handle


                -- We guess based on the cwd because who knows where temp
                -- files will get written to.
                cwd <- getCurrentDirectory
                cabal <- findCabal cwd
                let cabal' = maybe (error "No cabal file found") id cabal
                projs <- cabalSrcDirs cabal'
                config <- qqConfig

                translateLoaded projs [filepath] []
                    simplTranslationConfig
                    config)
    return exG2
    where
        modImports = map ("import " ++) 
                   . filter (`notElem` badImports)
                   . map (\(Module _ (ModName n)) -> n) $ mods
        badImports = ["G2.QuasiQuotes.QuasiQuotes"]


-- allDirectories :: FilePath -> IO [FilePath]
-- allDirectories fp = do
--     all <- listDirectory fp
--     fs <- filterM doesDirectoryExist all

--     fs' <- mapM allDirectories fs

--     mapM makeAbsolute $ fp:fs ++ concat fs'

-- | If a State has been completely symbolically executed (i.e. no states were
-- discarded by a Halter) we encoded it as Completed.
-- Otherwise, we encode the original State and Bindings as NonCompleted
data ExecOut = Completed [State ()] Bindings
             | NonCompleted (State ()) Bindings

runExecutionQ :: Merging -> State () -> Bindings -> Config -> Q ExecOut
runExecutionQ mergestates s b config = do
  runIO $ do
    let (s', b') = addAssume s b

    SomeSolver solver <- initSolverInfinite config
    let simplifier = IdSimplifier
    case qqRedHaltOrd config solver simplifier mergestates of
        (SomeReducer red, SomeHalter hal, SomeOrderer ord) -> do
            let (s'', b'') = runG2Pre [] s' b'
                hal' = hal :<~> ZeroHalter 20 :<~> LemmingsHalter
            (xs, b''') <- runExecutionToProcessed red hal' ord simplifier mergestates s'' b''

            case xs of
                Processed { accepted = acc, discarded = [] } -> do
                    let acc' = filter (trueCurrExpr) acc
                    return $ Completed acc' b'''
                _ -> do
                    return $ NonCompleted s'' b''
    where
        trueCurrExpr (State { curr_expr = CurrExpr _ e
                            , known_values = kv }) = e == mkTrue kv
        _ = False

-- | As soon as one States has been discarded, discard all States
data LemmingsHalter = LemmingsHalter

instance Halter LemmingsHalter () t where
    initHalt _ _ = ()
    updatePerStateHalt _ _ _ _ = ()
    discardOnStart _ _ pr _ = not . null . discarded $ pr
    stopRed _ _ _ _ = Continue
    stepHalter _ _ _ _ _ = ()

fileName :: String
fileName = "THTemp.hs"

moduleName :: String
moduleName = "THTemp"

functionName :: String
functionName = "g2Expr"

qqRedHaltOrd :: (Solver solver, Simplifier simplifier) => Config -> solver -> simplifier -> Merging -> (SomeReducer (), SomeHalter (), SomeOrderer ())
qqRedHaltOrd config solver simplifier mergeStates =
    let
        share = sharing config
        logSt = logStates config

        tr_ng = mkNameGen ()
        state_name = G2.Name "state" Nothing 0 Nothing
    in
    ( SomeReducer
        (NonRedPCRed :<~| TaggerRed state_name tr_ng)
            <~| (case logSt of
                    Just fp -> SomeReducer (StdRed share mergeStates solver simplifier :<~ Logger fp)
                    Nothing -> SomeReducer (StdRed share mergeStates solver simplifier))
    , SomeHalter
        (DiscardIfAcceptedTag state_name 
        :<~> AcceptHalter)
    , SomeOrderer NextOrderer)

addAssume :: State t -> Bindings -> (State t, Bindings)
addAssume s@(State { curr_expr = CurrExpr er e }) b@(Bindings { name_gen = ng }) =
    let
        (v, ng') = freshId (Ty.typeOf e) ng
        e' = Let [(v, e)] (Assume Nothing (Var v) (Var v))
    in
    (s { curr_expr = CurrExpr er e' }, b { name_gen = ng' })

type TypeEnvName = TH.Name
type CleanedNamesName = TH.Name

-- We have the TypeEnv separately from the state, and it is a waste to lift it to TH twice.
-- This avoids having to do that
moveOutTypeEnvState :: Data t => TypeEnvName -> State t -> Q Exp
moveOutTypeEnvState tenv_name s = do
    let s' = s { type_env = M.empty }
        s_exp = liftDataT s'
    [| $(s_exp) { type_env = $(varE tenv_name) } |]

-- Returns an Q Exp represeting a [(Name, Expr)] list
regVarBindings :: [TH.Name] -> TypeEnvName -> CleanedNamesName -> InputIds -> Bindings -> Q Exp
regVarBindings ns tenv_name cleaned_name is (Bindings { input_names = ins, cleaned_names = cleaned }) = do
    let ns_exp = map varE ns
        ty_ns_exp = map (\(n, i) -> sigE n (toTHType cleaned (Ty.typeOf i))) $ zip ns_exp is

        ins_exp = liftDataT ins

        g2Rep_exp = [| g2Rep $(varE tenv_name) $(varE cleaned_name) |]
        ns_expr = map (appE g2Rep_exp) ty_ns_exp

        zip_exp = [| zip $(ins_exp) $(listE ns_expr) |]
    zip_exp

-- | Adds the appropriate number of lambda bindings to the Exp,
-- and sets up a conversion from TH Exp's to G2 Expr's.
-- The returned Exp should have a function type and return type (State t).
addCompRegVarPasses :: StateListExp -> TypeEnvName -> CleanedNamesName -> [TH.Name] -> InputIds -> Bindings -> Q Exp
addCompRegVarPasses xs_exp tenv_name cleaned_name ns in_ids b = do

    let zip_exp = regVarBindings ns tenv_name cleaned_name in_ids b

        flooded_exp = appE (varE 'mapMaybe) (appE (varE 'floodConstantsChecking) zip_exp)

    appE flooded_exp xs_exp

addedNonCompRegVarBinds :: StateExp -> TypeEnvName -> CleanedNamesName -> [TH.Name] -> InputIds  -> Bindings -> Q Exp
addedNonCompRegVarBinds state_exp tenv_name cleaned_name ns in_ids b = do

    let zip_exp = regVarBindings ns tenv_name cleaned_name in_ids b

        flooded_exp = [| case floodConstantsChecking $(zip_exp) $(state_exp) of
                            Just s' -> s'
                            Nothing -> error "addedNonCompRegVarBinds: Nothing"|]

    flooded_exp

elimUnusedCompleted :: Named t => [State t] -> Bindings -> ([State t], Bindings)
elimUnusedCompleted xs b =
    let
        b' = b { deepseq_walkers = M.empty
               , higher_order_inst = [] }

        xs' = map (\s -> s { type_classes = initTypeClasses []
                           , rules = [] }) xs
        xs'' = map (fst . flip markAndSweepIgnoringKnownValues b') xs'
    in
    (xs'', b')

elimUnusedNonCompleted :: Named t => State t -> Bindings -> (State t, Bindings)
elimUnusedNonCompleted s b =
    let
        b' = b { deepseq_walkers = M.empty
               , higher_order_inst = [] }
        s' = s { type_classes = initTypeClasses []
               , rules = [] }
    in
    markAndSweepIgnoringKnownValues s' b'

type StateExp = Q Exp
type StateListExp = Q Exp
type BindingsExp = Q Exp

data ErrorHalter = ErrorHalter

instance Halter ErrorHalter () t where
    initHalt _ _ = ()
    updatePerStateHalt _ _ _ _ = ()

    stopRed _ _ _ (State { curr_expr = CurrExpr _ (G2.Prim Error _)}) = Discard
    stopRed _ _ _ _ = Continue

    stepHalter _ _ _ _ _ = ()

executeAndSolveStates :: Merging -> StateExp -> BindingsExp -> Q Exp
executeAndSolveStates mergeStates s b = do
    varE 'executeAndSolveStates' `appE` liftData mergeStates `appE` b `appE` s 

executeAndSolveStates' :: Merging -> Bindings -> State () -> IO (Maybe (ExecRes ()))
executeAndSolveStates' mergeStates b s = do
    config <- qqConfig
    SomeSolver solver <- initSolverInfinite config
    let simplifier = IdSimplifier
    case qqRedHaltOrd config solver simplifier mergeStates of
        (SomeReducer red, SomeHalter hal, _) -> do
            -- let hal' = hal :<~> ErrorHalter
            --                :<~> MaxOutputsHalter (Just 1)
            --                :<~> BranchAdjSwitchEveryNHalter 2000 20
                           -- :<~> SwitchEveryNHalter 2000
            let hal' = hal :<~> ErrorHalter :<~> VarLookupLimit 3 :<~> MaxOutputsHalter (Just 1)
            -- (res, _) <- runG2Post red hal' PickLeastUsedOrderer solver s b
            -- (res, _) <- runG2Post (red :<~ Logger "qq") hal' ((IncrAfterN 2000 SymbolicADTOrderer)
                                          -- :<-> BucketSizeOrderer 6) solver s b
                ord = ToOrderer (IncrAfterN 2000 ADTHeightOrderer :<-> BucketSizeOrderer 6)
            (res, _) <- runG2Post (red) hal' ord solver simplifier s b mergeStates
            -- (res, _) <- runG2Post (red) hal' (BucketSizeOrderer 3) solver s b

            case res of
                exec_res:_ -> return $ Just exec_res
                _ -> return Nothing

-- Takes an Exp representing a list of States, and returns an Exp representing an ExecRes
solveStates :: StateExp -> BindingsExp -> Q Exp
solveStates xs b = do
    varE 'solveStates' `appE` b `appE` xs 

solveStates' :: ( Named t
                , ASTContainer t Expr
                , ASTContainer t G2.Type) => Bindings -> [State t] -> IO (Maybe (ExecRes t))
solveStates' b xs = do
    config <- qqConfig
    SomeSolver solver <- initSolverInfinite config
    let simplifier = IdSimplifier
    solveStates'' solver simplifier b xs

solveStates'' :: ( Named t
                 , ASTContainer t Expr
                 , ASTContainer t G2.Type
                 , Solver solver
                 , Simplifier simplifier) => solver -> simplifier -> Bindings -> [State t] -> IO (Maybe (ExecRes t))
solveStates'' _ _ _ [] =return Nothing
solveStates'' sol simplifier b (s:xs) = do
    m_ex_res <- runG2Solving sol simplifier b s
    case m_ex_res of
        Just _ -> do
            return m_ex_res
        Nothing -> solveStates'' sol simplifier b xs

-- | Get the values of the symbolic arguments, and returns them in a tuple
extractArgs :: InputIds -> CleanedNames -> TypeEnvName -> Q Exp -> Q Exp
extractArgs in_ids cleaned tenv es =
    [|do
        r <- $(es)
        case r of
            Just r' -> return . Just . $(toSymbArgsTuple in_ids cleaned tenv) $ conc_args r'
            Nothing -> return Nothing |]

-- | Returns a function to turn the first (length of InputIds) elements of a list into a tuple
toSymbArgsTuple :: InputIds -> CleanedNames -> TypeEnvName -> Q Exp
toSymbArgsTuple in_ids cleaned tenv_name = do
    lst <- newName "lst"

    lamE [varP lst]
        (tupE $ map (\(i, n) -> [| g2UnRep $(varE tenv_name) ($(varE lst) !! n) :: $(toTHType cleaned (Ty.typeOf i)) |]) $ zip in_ids ([0..] :: [Int]))

qqConfig :: IO Config
qqConfig = do
  homedir <- getHomeDirectory
  -- return $ mkConfig homedir ["--log-states", "../Debugging Output/g2q/sumEvensFullSM"] M.empty
  return $ mkConfig homedir [] M.empty
