{-# LANGUAGE OverloadedStrings #-}

-- | Haskell Translation
module G2.Internals.Translation.Haskell
    ( CompileClosure
    , loadProj
    , mkCompileClosure
    , hskToG2
    , mkIOString
    , prim_list
    , rawDump
    , mkId
    , mkIdUnsafe
    , mkName
    , mkTyConName
    , mkData
    , mkSpan
    , mkRealSpan
    , absVarLoc
    , NameMap
    , TypeNameMap
    ) where

import qualified G2.Internals.Language as G2

import Avail
import qualified Class as C
import Coercion
import CoreSyn
import DataCon
import DynFlags
import FastString
import GHC
import GHC.Paths
import HscMain
import HscTypes
import InstEnv
import Literal
import Name
import Outputable
import Pair
import SrcLoc
import TidyPgm
import TyCon
import TyCoRep
import Unique
import Var as V

import qualified Data.Array as A
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import System.Directory


mkIOString :: (Outputable a) => a -> IO String
mkIOString obj = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    return (showPpr dflags obj)

mkRawCore :: FilePath -> IO CoreModule
mkRawCore fp = runGhc (Just libdir) $ do
    _ <- setSessionDynFlags =<< getSessionDynFlags
    -- compileToCoreModule fp
    compileToCoreSimplified fp

rawDump :: FilePath -> IO ()
rawDump fp = do
  core <- mkRawCore fp
  str <- mkIOString core
  putStrLn str

type NameMap = HM.HashMap (T.Text, Maybe T.Text) G2.Name
type TypeNameMap = HM.HashMap (T.Text, Maybe T.Text) G2.Name

equivMods :: HM.HashMap T.Text T.Text
equivMods = HM.fromList
            [ ("GHC.Classes2", "GHC.Classes")
            , ("GHC.Types2", "GHC.Types")
            , ("GHC.Integer2", "GHC.Integer")
            , ("GHC.Integer.Type2", "GHC.Integer.Type")
            , ("GHC.Prim2", "GHC.Prim")
            , ("GHC.Tuple2", "GHC.Tuple")
            , ("GHC.Magic2", "GHC.Magic")
            , ("GHC.CString2", "GHC.CString")
            , ("Data.Map.Base", "Data.Map")]

hskToG2 :: Maybe HscTarget -> FilePath -> FilePath -> NameMap -> TypeNameMap -> Bool -> 
    IO (Maybe String, G2.Program, [G2.ProgramType], [(G2.Name, G2.Id, [G2.Id])], NameMap, TypeNameMap, [String], [ExportedName])
hskToG2 hsc proj src nm tm simpl = do
    (mb_modname, sums_gutss, _, c, m_dets, ex) <- mkCompileClosure hsc proj src simpl
    
    let (nm2, binds) = mapAccumR (\nm' (_, _, b, br) -> mapAccumR (\v -> mkBinds v tm br) nm' b) nm sums_gutss
    let binds' = concat binds

    let m_dets_tycon = map (typeEnvTyCons . md_types) m_dets
    let ((nm3, tm2), tycons) = mapAccumR (\(nm', tm') (_, t, _, _) -> mapAccumR (uncurry mkTyCon) (nm', tm') t) (nm2, tm) sums_gutss
    let ((nm4, tm3), tycons') = mapAccumR (\(nm', tm') t -> mapAccumR (uncurry mkTyCon) (nm', tm') t) (nm3, tm2) m_dets_tycon
    let tycons'' = catMaybes $ concat tycons ++ concat tycons'

    let classes = map (mkClass tm2) c

    let tgt_lhs = map (occNameString . nameOccName . V.varName) $
          filter ((== mb_modname) . fmap (moduleNameString . moduleName) . nameModule_maybe . V.varName) $
          concatMap bindersOf $
          concatMap (\(_, _, bs, _) -> bs) sums_gutss

    return (mb_modname, binds', tycons'', classes, nm4, tm3, tgt_lhs, ex)

type ExportedName = G2.Name
type CompileClosure = (Maybe String, [(ModSummary, [TyCon], [CoreBind], Maybe ModBreaks)], HscEnv, [ClsInst], [ModDetails], [ExportedName])

loadProj ::  Maybe HscTarget -> FilePath -> FilePath -> [GeneralFlag] -> Bool -> Ghc SuccessFlag
loadProj hsc proj src gflags simpl = do
    beta_flags <- getSessionDynFlags
    let gen_flags = gflags
    let beta_flags' = foldl' gopt_set beta_flags gen_flags
    let dflags = beta_flags' { hscTarget = case hsc of
                                                Just hsc' -> hsc'
                                                _ -> hscTarget beta_flags'
                             , importPaths = [proj]
                             , ufCreationThreshold = if simpl then ufCreationThreshold beta_flags' else -5000
                             , ufUseThreshold = if simpl then ufUseThreshold beta_flags' else -5000
                             , ufFunAppDiscount = if simpl then ufFunAppDiscount beta_flags' else -5000
                             , ufDictDiscount = if simpl then ufDictDiscount beta_flags' else -5000
                             , ufKeenessFactor = if simpl then ufKeenessFactor beta_flags' else -5000
                             , hpcDir = proj}

    

    _ <- setSessionDynFlags dflags
    target <- guessTarget src Nothing
    _ <- setTargets [target]
    load LoadAllTargets

mkCompileClosure :: Maybe HscTarget -> FilePath -> FilePath -> Bool -> IO CompileClosure
mkCompileClosure hsc proj src simpl = do
    (mb_modname, mod_graph, mod_gutss, mod_breaks, env) <- runGhc (Just libdir) $ do
        _ <- loadProj hsc proj src [] simpl
        env <- getSession
        -- Now that things are loaded, make the compilation closure.
        mod_graph <- getModuleGraph
        pmods <- mapM parseModule mod_graph
        tmods <- mapM typecheckModule pmods
        dmods <- mapM desugarModule tmods
        let mod_gutss = map coreModule dmods
        let mod_breaks = map mg_modBreaks mod_gutss

        let mb_modname = listToMaybe $ map (moduleNameString . moduleName . ms_mod)
                                     $ filter ((== Just src) . ml_hs_file . ms_location)
                                     $ map (pm_mod_summary) pmods

        return (mb_modname, mod_graph, mod_gutss, mod_breaks, env)

    -- Perform simplification and tidying, which is necessary for getting the
    -- typeclass selector functions.
    smpl_gutss <- mapM (hscSimplify env) mod_gutss
    tidy_pgms <- mapM (tidyProgram env) smpl_gutss-- (if simpl then smpl_gutss else mod_gutss)
    let cg_gutss = map fst tidy_pgms
    let tcss_pgms = map (\c -> (cg_tycons c, cg_binds c)) cg_gutss
    let (tcss, bindss) = unzip tcss_pgms

    let mod_dets = map snd tidy_pgms

    -- Get TypeClasses
    let cls_insts = concatMap mg_insts mod_gutss

    let exported = concatMap exportedNames mod_gutss

    return (mb_modname, zip4 mod_graph tcss bindss mod_breaks, env, cls_insts, mod_dets, exported)

mkBinds :: NameMap -> TypeNameMap -> Maybe ModBreaks -> CoreBind -> (NameMap, [(G2.Id, G2.Expr)])
mkBinds nm tm mb (NonRec var expr) = 
    let
        (i, nm') = mkIdUpdatingNM var nm tm
    in
    (nm', [(i, mkExpr nm' tm mb expr)])
mkBinds nm tm mb (Rec ves) =
    mapAccumR (\nm' (v, e) ->
                let
                    (i, nm'') = mkIdUpdatingNM v nm' tm
                in
                (nm'', (i, mkExpr nm'' tm mb e))
            ) nm ves

mkExpr :: NameMap -> TypeNameMap -> Maybe ModBreaks -> CoreExpr -> G2.Expr
mkExpr nm tm _ (Var var) = G2.Var (mkIdLookup var nm tm)
mkExpr _ _ _ (Lit lit) = G2.Lit (mkLit lit)
mkExpr nm tm mb (App fxpr axpr) = G2.App (mkExpr nm tm mb fxpr) (mkExpr nm tm mb axpr)
mkExpr nm tm mb (Lam var expr) = G2.Lam (mkId tm var) (mkExpr nm tm mb expr)
mkExpr nm tm mb (Let bnd expr) = G2.Let (mkBind nm tm mb bnd) (mkExpr nm tm mb expr)
mkExpr nm tm mb (Case mxpr var _ alts) = G2.Case (mkExpr nm tm mb mxpr) (mkId tm var) (mkAlts nm tm mb alts)
mkExpr nm tm mb (Cast expr c) =  G2.Cast (mkExpr nm tm mb expr) (mkCoercion tm c)
mkExpr _  tm _ (Coercion c) = G2.Coercion (mkCoercion tm c)
mkExpr nm tm mb (Tick t expr) =
    case createTickish mb t of
        Just t' -> G2.Tick t' $ mkExpr nm tm mb expr
        Nothing -> mkExpr nm tm mb expr
mkExpr _ tm _ (Type ty) = G2.Type (mkType tm ty)

createTickish :: Maybe ModBreaks -> Tickish i -> Maybe G2.Tickish
createTickish (Just mb) (Breakpoint {breakpointId = bid}) =
    case mkSpan $ modBreaks_locs mb A.! bid of
        Just s -> Just $ G2.Breakpoint $ s
        Nothing -> Nothing
createTickish _ _ = Nothing

mkId :: TypeNameMap -> Id -> G2.Id
mkId tm vid = G2.Id ((mkName . V.varName) vid) ((mkType tm . varType) vid)

-- Makes an Id, not respecting UniqueIds
mkIdUnsafe :: Id -> G2.Id
mkIdUnsafe vid = G2.Id ((mkName . V.varName) vid) (mkType HM.empty . varType $ vid)

mkIdLookup :: Id -> NameMap -> TypeNameMap -> G2.Id
mkIdLookup i nm tm =
    let
        n = mkNameLookup (V.varName i) nm
        t = mkType tm . varType $ i
    in
    G2.Id n t

mkIdUpdatingNM :: Id -> NameMap -> TypeNameMap -> (G2.Id, NameMap)
mkIdUpdatingNM vid nm tm =
    let
        n@(G2.Name n' m _ _) = mkName . V.varName $ vid
        i = G2.Id n ((mkType tm . varType) vid)

        nm' = HM.insert (n', m) n nm
    in
    (i, nm')

mkName :: Name -> G2.Name
mkName name = G2.Name occ mdl unq sp
  where
    occ = T.pack . occNameString . nameOccName $ name
    unq = (getKey . nameUnique) name
    mdl = case nameModule_maybe name of
              Nothing -> Nothing
              Just md -> switchModule (T.pack . moduleNameString . moduleName $ md)

    sp = mkSpan $ getSrcSpan name

mkNameLookup :: Name -> NameMap -> G2.Name
mkNameLookup name nm =
    -- We only lookup in the NameMap if the Module name is not Nothing
    -- Internally, a module may use multiple variables with the same name and a module Nothing
    case mdl of
        Nothing -> G2.Name occ mdl unq sp
        _ -> case HM.lookup (occ, mdl) nm of
                Just (G2.Name n' m i _) -> G2.Name n' m i sp
                Nothing -> G2.Name occ mdl unq sp
    where
        occ = T.pack . occNameString . nameOccName $ name
        unq = getKey . nameUnique $ name
        mdl = case nameModule_maybe name of
                  Nothing -> Nothing
                  Just md -> switchModule (T.pack . moduleNameString . moduleName $ md)

        sp = mkSpan $ getSrcSpan name

mkSpan :: SrcSpan -> Maybe G2.Span
mkSpan (RealSrcSpan s) = Just $ mkRealSpan s
mkSpan _ = Nothing

mkRealSpan :: RealSrcSpan -> G2.Span
mkRealSpan s =
    let
        st = mkRealLoc $ realSrcSpanStart s
        en = mkRealLoc $ realSrcSpanEnd s
    in
    G2.Span { G2.start = st
            , G2.end = en}

mkRealLoc :: RealSrcLoc -> G2.Loc
mkRealLoc l =
    G2.Loc { G2.line = srcLocLine l
           , G2.col = srcLocCol l
           , G2.file = unpackFS $ srcLocFile l}

switchModule :: T.Text -> Maybe T.Text
switchModule m =
    case HM.lookup m equivMods of
        Just m'' -> Just m''
        Nothing -> Just m

mkLit :: Literal -> G2.Lit
mkLit (MachChar chr) = G2.LitChar chr
mkLit (MachStr bstr) = G2.LitString (C.unpack bstr)
mkLit (MachInt i) = G2.LitInt (fromInteger i)
mkLit (MachInt64 i) = G2.LitInt (fromInteger i)
mkLit (MachWord i) = G2.LitInt (fromInteger i)
mkLit (MachWord64 i) = G2.LitInt (fromInteger i)
mkLit (MachFloat rat) = G2.LitFloat rat
mkLit (MachDouble rat) = G2.LitDouble rat
mkLit (LitInteger i _) = G2.LitInteger (fromInteger i)
mkLit (MachNullAddr) = error "mkLit: MachNullAddr"
mkLit (MachLabel _ _ _ ) = error "mkLit: MachLabel"

mkBind :: NameMap -> TypeNameMap -> Maybe ModBreaks -> CoreBind -> [(G2.Id, G2.Expr)]
mkBind nm tm mb (NonRec var expr) = [(mkId tm var, mkExpr nm tm mb expr)]
mkBind nm tm mb (Rec ves) = map (\(v, e) -> (mkId tm v, mkExpr nm tm mb e)) ves

mkAlts :: NameMap -> TypeNameMap -> Maybe ModBreaks -> [CoreAlt] -> [G2.Alt]
mkAlts nm tm mb = map (mkAlt nm tm mb)

mkAlt :: NameMap -> TypeNameMap -> Maybe ModBreaks -> CoreAlt -> G2.Alt
mkAlt nm tm mb (acon, prms, expr) = G2.Alt (mkAltMatch nm tm acon prms) (mkExpr nm tm mb expr)

mkAltMatch :: NameMap -> TypeNameMap -> AltCon -> [Var] -> G2.AltMatch
mkAltMatch nm tm (DataAlt dcon) params = G2.DataAlt (mkData nm tm dcon) (map (mkId tm) params)
mkAltMatch _ _ (LitAlt lit) _ = G2.LitAlt (mkLit lit)
mkAltMatch _ _ (DEFAULT) _ = G2.Default

mkType :: TypeNameMap -> Type -> G2.Type
mkType tm (TyVarTy v) = G2.TyVar $ mkId tm v-- (mkName (V.varName v)) (mkType (varType v))
mkType tm (AppTy t1 t2) = G2.TyApp (mkType tm t1) (mkType tm t2)
mkType tm (ForAllTy (Anon t) ty) = G2.TyFun (mkType tm t) (mkType tm ty)
mkType tm (ForAllTy b ty) = G2.TyForAll (mkTyBinder tm b) (mkType tm ty)
mkType _ (LitTy _) = G2.TyBottom
mkType _ (CastTy _ _) = error "mkType: CastTy"
mkType _ (CoercionTy _) = error "mkType: Coercion"
mkType tm (TyConApp tc ts) = if not (isFunTyCon tc) || (length ts /= 2)
    then G2.TyConApp (mkTyConName tm tc) (map (mkType tm) ts)
    else case ts of
        (t1:t2:[]) -> G2.TyFun (mkType tm t1) (mkType tm t2)
        _ -> error "mkType: non-arity 2 FunTyCon from GHC"

mkTyCon :: NameMap -> TypeNameMap -> TyCon -> ((NameMap, TypeNameMap), Maybe G2.ProgramType)
mkTyCon nm tm t = case dcs of
                        Just dcs' -> ((nm'', tm''), Just (n, dcs'))
                        Nothing -> ((nm'', tm''), Nothing)
  where
    n@(G2.Name n' m _ _) = mkName . tyConName $ t
    tm' = HM.insert (n', m) n tm

    nm' = foldr (uncurry HM.insert) nm
            $ map (\n_@(G2.Name n'_ m_ _ _) -> ((n'_, m_), n_)) 
            $ map (flip mkNameLookup nm . dataConName) $ visibleDataCons (algTyConRhs t)

    bv = map (mkName . V.varName) $ tyConTyVars t

    (nm'', tm'', dcs, dcsf) = case isAlgTyCon t of 
                            True -> case algTyConRhs t of
                                            DataTyCon { data_cons = dc } -> 
                                                ( nm'
                                                , tm'
                                                , Just $ G2.DataTyCon bv $ map (mkData nm' tm) dc
                                                , Just $ map (mkId tm'' . dataConWorkId) dc)
                                            NewTyCon { data_con = dc
                                                     , nt_rhs = rhst} -> 
                                                     ( nm'
                                                     , tm'
                                                     , Just $ G2.NewTyCon { G2.bound_names = bv
                                                                          , G2.data_con = mkData nm' tm dc
                                                                          , G2.rep_type = mkType tm rhst}
                                                     , Just $ [(mkId tm'' . dataConWorkId) dc])
                                            AbstractTyCon {} -> error "Unhandled TyCon AbstractTyCon"
                                            TupleTyCon {} -> error "Unhandled TyCon TupleTyCon"
                            False -> case isTypeSynonymTyCon t of
                                    True -> 
                                        let
                                            st = fromJust $ synTyConRhs_maybe t
                                            st' = mkType tm st
                                        in
                                        (nm, tm, Just $ G2.TypeSynonym {G2.synonym_of = st'}, Nothing)
                                    False -> (nm, tm, Nothing, Nothing)
    -- dcs = if isDataTyCon t then map mkData . data_cons . algTyConRhs $ t else []

mkTyConName :: TypeNameMap -> TyCon -> G2.Name
mkTyConName tm tc =
    let
        n@(G2.Name n' m _ l) = mkName $ tyConName tc
    in
    case HM.lookup (n', m) tm of
    Just (G2.Name n'' m' i _) -> G2.Name n'' m' i l
    Nothing -> n

mkData :: NameMap -> TypeNameMap -> DataCon -> G2.DataCon
mkData nm tm datacon = G2.DataCon name ty tys
  where
    name = mkDataName nm datacon
    ty = (mkType tm . dataConRepType) datacon
    tys  = map (mkType tm) (dataConOrigArgTys datacon)

mkDataName :: NameMap -> DataCon -> G2.Name
mkDataName nm datacon = (flip mkNameLookup nm . dataConName) datacon

mkTyBinder :: TypeNameMap -> TyBinder -> G2.TyBinder
mkTyBinder tm (Anon t) = G2.AnonTyBndr (mkType tm t)
mkTyBinder tm (Named v _) = G2.NamedTyBndr (mkId tm v)

prim_list :: [String]
prim_list = [">=", ">", "==", "/=", "<=", "<",
             "&&", "||", "not",
             "+", "-", "*", "/", "implies", "negate", "error", "iff" ]


mkCoercion :: TypeNameMap -> Coercion -> G2.Coercion
mkCoercion tm c =
    let
        k = fmap (mkType tm) $ coercionKind c
    in
    (pFst k) G2.:~ (pSnd k)

mkClass :: TypeNameMap -> ClsInst -> (G2.Name, G2.Id, [G2.Id])
mkClass tm (ClsInst { is_cls = c, is_dfun = dfun }) = 
    (flip mkNameLookup tm . C.className $ c, mkId tm dfun, map (mkId tm) $ C.classTyVars c)


exportedNames :: ModGuts -> [ExportedName]
exportedNames = concatMap availInfoNames . mg_exports

availInfoNames :: AvailInfo -> [ExportedName]
availInfoNames (Avail _ n) = [mkName n]
availInfoNames (AvailTC n ns _) = mkName n:map mkName ns

-- | absVarLoc'
-- Switches all file paths in Var namesand Ticks to be absolute
absVarLoc :: G2.Program -> IO G2.Program
absVarLoc = 
    mapM 
        (mapM (\(i, e) -> do 
                    e' <- absVarLoc' e
                    return (i, e')
              )
        )

absVarLoc' :: G2.Expr -> IO G2.Expr
absVarLoc' (G2.Var (G2.Id (G2.Name n m i (Just s)) t)) = do
    return $ G2.Var $ G2.Id (G2.Name n m i (Just $ s)) t
absVarLoc' (G2.App e1 e2) = do
    e1' <- absVarLoc' e1
    e2' <- absVarLoc' e2
    return $ G2.App e1' e2'
absVarLoc' (G2.Lam i e) = return . G2.Lam i =<< absVarLoc' e
absVarLoc' (G2.Let b e) = do
    b' <- mapM (\(i, be) -> do
                    be' <- absVarLoc' be
                    return (i, be')
               ) b
    e' <- absVarLoc' e
    return $ G2.Let b' e'
absVarLoc' (G2.Case e i as) = do
    e' <- absVarLoc' e
    as' <- mapM (\(G2.Alt a ae) -> return . G2.Alt a =<< absVarLoc' ae) as
    return $ G2.Case e' i as'
absVarLoc' (G2.Cast e c) = do
    e' <- absVarLoc' e
    return $ G2.Cast e' c
absVarLoc' (G2.Tick (G2.Breakpoint s) e) = do
    s' <- absLocSpan s
    let t' = G2.Breakpoint s'

    e' <- absVarLoc' e
    return $ G2.Tick t' e'
absVarLoc' (G2.Assume e1 e2) = do
    e1' <- absVarLoc' e1
    e2' <- absVarLoc' e2
    return $ G2.Assume e1' e2'
absVarLoc' (G2.Assert fc e1 e2) = do
    e1' <- absVarLoc' e1
    e2' <- absVarLoc' e2
    return $ G2.Assert fc e1' e2'
absVarLoc' e = return e

absLocSpan :: G2.Span -> IO G2.Span
absLocSpan s@G2.Span {G2.start = st, G2.end = en} = do
    st' <- absLoc st
    en' <- absLoc en
    return $ s {G2.start = st', G2.end = en'}

absLoc :: G2.Loc -> IO G2.Loc
absLoc l@G2.Loc {G2.file = f} = do
    f' <- makeAbsolute f
    return $ l {G2.file = f'}
