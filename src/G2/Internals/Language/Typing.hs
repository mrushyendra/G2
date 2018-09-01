{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type Checker
--   Provides type checking capabilities over G2 Language.
module G2.Internals.Language.Typing
    ( Typed (..)
    , PresType (..)
    , tyInt
    , tyInteger
    , tyDouble
    , tyFloat
    , tyBool
    , mkTyApp
    , mkTyFun
    , tyAppCenter
    , tyAppArgs
    , unTyApp
    , mkTyConApp
    , (.::)
    , (.::.)
    , specializes
    , hasFuncType
    , appendType
    , higherOrderFuncs
    , isAlgDataTy
    , isTYPE
    , isTyFun
    , hasTYPE
    , isTyVar
    , hasTyBottom
    , tyVars
    , hasTyFuns
    , isPolyFunc
    , numArgs
    , ArgType (..)
    , argumentTypes
    , spArgumentTypes
    , tyForAllBindings
    , anonArgumentTypes
    , returnType
    , polyIds
    , splitTyForAlls
    , splitTyFuns
    , retype
    , nestTyForAlls
    , inTyForAlls
    ) where

import G2.Internals.Language.AST
import qualified G2.Internals.Language.KnownValues as KV
import G2.Internals.Language.Syntax

import qualified Data.Map as M
import Data.Monoid hiding (Alt)

import Debug.Trace

tyInt :: KV.KnownValues -> Type
tyInt kv = TyConApp (KV.tyInt kv) (tyTYPE kv)

tyInteger :: KV.KnownValues -> Type
tyInteger kv = TyConApp (KV.tyInteger kv) (tyTYPE kv)

tyDouble :: KV.KnownValues -> Type
tyDouble kv = TyConApp (KV.tyDouble kv) (tyTYPE kv)

tyFloat :: KV.KnownValues -> Type
tyFloat kv = TyConApp (KV.tyFloat kv) (tyTYPE kv)

tyBool :: KV.KnownValues -> Type
tyBool kv = TyConApp (KV.tyBool kv) (tyTYPE kv)

tyTYPE :: KV.KnownValues -> Type
tyTYPE _ = TYPE

mkTyApp' :: Type -> Type -> Type
mkTyApp' (TyFun _ t2) _ = t2
mkTyApp' t1 t2 = TyApp t1 t2

-- | mkTyFun
-- Turns the Expr list into an application spine
mkTyFun :: [Type] -> Type
mkTyFun [] = error "mkTyFun: empty list"
mkTyFun [t] = t
mkTyFun (t1:ts) = TyFun t1 (mkTyFun ts)
-- mkTyFun (t:[]) = t
-- mkTyFun (t1:t2:ts) = mkTyFun (TyFun t1 t2 : ts)

tyAppCenter :: Type -> Type
tyAppCenter (TyApp t _) = tyAppCenter t
tyAppCenter t = t

tyAppArgs :: Type -> [Type]
tyAppArgs (TyApp t t') = tyAppArgs t ++ [t']
tyAppArgs t = []

mkTyApp :: [Type] -> Type
mkTyApp [] = TYPE
mkTyApp (t:[]) = t
mkTyApp (t1:t2:ts) = mkTyApp (TyApp t1 t2 : ts)

mkTyConApp :: Name
           -> [Type] -- ^ Type arguments
           -> Kind -- ^ Result kind
           -> Type
mkTyConApp n ts k =
    let
        tsk = mkTyApp $ map typeOf ts ++ [k]
    in
    mkTyApp $ TyConApp n tsk:ts

-- | unTyApp
-- Unravels the application spine.
unTyApp :: Type -> [Type]
unTyApp (TyApp t t') = unTyApp t ++ [t']
unTyApp t = [t]

-- | Typed typeclass.
class Typed a where
    typeOf :: a -> Type
    typeOf = fst . typeOf' M.empty

    typeOf' :: M.Map Name Type -> a -> (Type, M.Map Name Type)

-- | `Id` instance of `Typed`.
instance Typed Id where
    typeOf' m (Id _ ty) = (tyVarRename m ty, m)

-- | `Primitive` instance of `Typed`
-- | `Lit` instance of `Typed`.
instance Typed Lit where
    typeOf (LitInt _) = TyLitInt
    typeOf (LitFloat _) = TyLitFloat
    typeOf (LitDouble _) = TyLitDouble
    typeOf (LitChar _)   = TyLitChar
    typeOf (LitString _) = TyLitString
    typeOf (LitInteger _) = TyLitInt

    typeOf' m t = (typeOf t, m)

-- | `DataCon` instance of `Typed`.
instance Typed DataCon where
    typeOf' m (DataCon _ ty) = (ty, m)

-- | `Alt` instance of `Typed`.
instance Typed Alt where
    typeOf' m (Alt _ expr) = typeOf' m expr

-- | `Expr` instance of `Typed`.
instance Typed Expr where
    typeOf' m (Var v) = typeOf' m v
    typeOf' m (Lit lit) = typeOf' m lit
    typeOf' m (Prim _ ty) = (ty, m)
    typeOf' m (Data dcon) = typeOf' m dcon
    typeOf' m a@(App _ _) =
        let
            as = passedArgs a
            (t, _) = typeOf' m $ appCenter a
        in
        (appTypeOf m t as, m)
    typeOf' m (Lam u b e) =
        case u of
            TypeL -> (TyForAll (NamedTyBndr b) (fst $ typeOf' m e), m)
            TermL -> (TyFun (fst $ typeOf' m b) (fst $ typeOf' m e), m)
    typeOf' m (Let _ expr) = typeOf' m expr
    typeOf' m (Case _ _ (a:_)) = typeOf' m a
    typeOf' m (Case _ _ []) = (TyBottom, m)
    typeOf' m (Type ty) = (TYPE, m)
    typeOf' m (Cast _ (_ :~ t')) = (t', m)
    typeOf' m (Coercion (_ :~ t')) = (t', m)
    typeOf' m (Tick _ e) = typeOf' m e
    typeOf' m (Assert _ _ e) = typeOf' m e
    typeOf' m (Assume _ e) = typeOf' m e

appTypeOf :: M.Map Name Type -> Type -> [Expr] -> Type
appTypeOf m (TyForAll (NamedTyBndr i) t) (Type t':e) =
    let
        m' = M.insert (idName i) t' m
    in
    appTypeOf m (tyVarRename m' t) e
appTypeOf m' (TyFun _ t) (e:es) = appTypeOf m' t es
appTypeOf _ t [] = t
appTypeOf _ t es = error ("appTypeOf\n" ++ show t ++ "\n" ++ show es ++ "\n\n")

instance Typed Type where
    typeOf' m v@(TyVar (Id _ t)) = (t, m)
    typeOf' m (TyFun t1 t2) = (TYPE, m)
    typeOf' m (TyApp t1 t2) =
        let
            (ft, _) = typeOf' m t1
            (at, _) = typeOf' m t2
        in
        case (ft, at) of
            (ta@(TyFun _ t2'), _) -> (t2', m)
            (ta@(TyApp t1' _), _) -> (t1', m)
            (t, _) -> error $ "Overapplied Type\n" ++ show t1 ++ "\n" ++ show t2 ++ "\n\n" ++ show ft ++ "\n" ++ show at
    typeOf' m (TyConApp n t) = (t, m)
    typeOf' m (TyForAll (NamedTyBndr b) t) = (TyApp (typeOf b) (fst $ typeOf' m t), m)
    typeOf' m (TyForAll _ t) = typeOf' m t
    typeOf' m t = (t, m)

newtype PresType = PresType Type

instance Typed PresType where
    typeOf' m (PresType t) = (t, m)

-- | Retyping
-- We look to see if the type we potentially replace has a TyVar whose Id is a
-- match on the target key that we want to replace.
retype :: (ASTContainer m Type, Show m) => Id -> Type -> m -> m
retype key new e = modifyContainedASTs (retype' key new) $ e

retype' :: Id -> Type -> Type -> Type
retype' key new (TyVar test) = if key == test then new else TyVar test
retype' key new (TyForAll (NamedTyBndr nid) ty) =
  if key == nid
    then modifyChildren (retype' key new) ty
    else TyForAll (NamedTyBndr nid) (modifyChildren (retype' key new) ty)
retype' key new ty = modifyChildren (retype' key new) ty

tyVarRename :: (ASTContainer t Type) => M.Map Name Type -> t -> t
tyVarRename m = modifyASTsFix (tyVarRename' m)

tyVarRename' :: M.Map Name Type -> Type -> Type
tyVarRename' m t@(TyVar (Id n _)) = M.findWithDefault t n m
tyVarRename' _ t = t

-- | (.::)
-- Returns if the first type given is a specialization of the second,
-- i.e. if given t1, t2, returns true iff t1 :: t2
(.::) :: Typed t => t -> Type -> Bool
t1 .:: t2 = fst $ specializes M.empty (typeOf t1) t2

(.::.) :: Type -> Type -> Bool
t1 .::. t2 = fst (specializes M.empty t1 t2) && fst (specializes M.empty t2 t1)

specializes :: M.Map Name Type -> Type -> Type -> (Bool, M.Map Name Type)
specializes m _ TYPE = (True, m)
specializes m t (TyVar (Id n _)) =
    case M.lookup n m of
        Just (TyVar _) -> (True, m)
        Just t' -> specializes m t t'
        Nothing -> (True, M.insert n t m)
specializes m (TyFun t1 t2) (TyFun t1' t2') =
    let
        (b1, m') = specializes m t1 t1'
        (b2, m'') = specializes m' t2 t2'
    in
    (b1 && b2, m'')
specializes m (TyApp t1 t2) (TyApp t1' t2') =
    let
        (b1, m') = specializes m t1 t1'
        (b2, m'') = specializes m' t2 t2'
    in
    (b1 && b2, m'')
specializes m (TyConApp n ts) (TyConApp n' ts') =
    let
        (b, m') = specializes m ts ts'
    in
    (n == n' && b, m')
-- specializes m (TyConApp n ts) app@(TyApp _ _) =
--     let
--         appts = unTyApp app
--     in
--     case appts of
--         TyConApp n' ts':ts'' -> specializes m (TyConApp n ts) (TyConApp n' $ ts' ++ ts'')
--         _ -> (False, m)
-- specializes m app@(TyApp _ _) (TyConApp n ts) =
--     let
--         appts = unTyApp app
--     in
--     case appts of
--         TyConApp n' ts':ts'' -> specializes m (TyConApp n ts) (TyConApp n' $ ts' ++ ts'')
--         _ -> (False, m)

specializes m (TyFun t1 t2) (TyForAll (AnonTyBndr t1') t2') =
  let
      (b1, m') = specializes m t1 t1'
      (b2, m'') = specializes m' t2 t2'
  in (b1 && b2, m'')
specializes m (TyFun t1 t2) (TyForAll (NamedTyBndr _) t2') =
  specializes m (TyFun t1 t2) t2'
specializes m (TyForAll (AnonTyBndr t1) t2) (TyFun t1' t2') =
  let
      (b1, m') = specializes m t1 t1'
      (b2, m'') = specializes m' t2 t2'
  in (b1 && b2, m'')
specializes m (TyForAll (AnonTyBndr t1) t2) (TyForAll (AnonTyBndr t1') t2') =
  let
      (b1, m') = specializes m t1 t1'
      (b2, m'') = specializes m' t2 t2'
  in (b1 && b2, m'')
specializes m (TyForAll (AnonTyBndr t1) t2) (TyForAll (NamedTyBndr _) t2') =
  specializes m (TyForAll (AnonTyBndr t1) t2) t2'
specializes m (TyForAll (NamedTyBndr (Id _ t1)) t2) (TyForAll (NamedTyBndr (Id _ t1')) t2') =
  let
      (b1, m') = specializes m t1 t1'
      (b2, m'') = specializes m' t2 t2'
  in (b1 && b2, m'')
specializes m t (TyForAll _ t') =
  specializes m t t'
specializes m TyUnknown _ = (True, m)
specializes m _ TyUnknown = (True, m)
specializes m TyBottom _ = (True, m)
specializes m _ TyBottom = (False, m)
specializes m t1 t2 = (t1 == t2, m)

hasFuncType :: (Typed t) => t -> Bool
hasFuncType t =
    case typeOf t of
        (TyFun _ _) -> True
        (TyForAll _ _)  -> True
        _ -> False

-- | appendType
-- Converts the (function) type t1 to return t2
-- appendType (a -> b) c = (a -> b -> c)
appendType :: Type -> Type -> Type
appendType (TyFun t t1) t2 = TyFun t (appendType t1 t2)
appendType t1 t2 = TyFun t1 t2

-- | higherOrderFuncs
-- Returns all internal higher order function types
higherOrderFuncs :: Typed t => t -> [Type]
higherOrderFuncs = higherOrderFuncs' . typeOf

higherOrderFuncs' :: Type -> [Type]
higherOrderFuncs' = eval higherOrderFuncs''

higherOrderFuncs'' :: Type -> [Type]
higherOrderFuncs'' (TyFun t@(TyFun _ _) _) = [t]
higherOrderFuncs'' _ = []

-- | isAlgDataTy
isAlgDataTy :: Typed t => t -> Bool
isAlgDataTy = isAlgDataTy' . typeOf

isAlgDataTy' :: Type -> Bool
isAlgDataTy' (TyConApp _ _) = True
isAlgDataTy' _ = False

isTYPE :: Type -> Bool
isTYPE TYPE = True
isTYPE (TyConApp (Name "TYPE" _ _ _) _) = True
isTYPE _ = False

hasTYPE :: Type -> Bool
hasTYPE TYPE = True
hasTYPE (TyConApp (Name "TYPE" _ _ _) _) = True
hasTYPE (TyFun t t') = hasTYPE t || hasTYPE t'
hasTYPE (TyApp t t') = hasTYPE t || hasTYPE t'
hasTYPE _ = False

isTyVar :: Type -> Bool
isTyVar (TyVar _) = True
isTyVar _ = False

isTyFun :: Type -> Bool
isTyFun (TyFun _ _) = True
isTyFun _ = False

-- | isPolyFunc
-- Checks if the given function is a polymorphic function
isPolyFunc ::  Typed t => t -> Bool
isPolyFunc = isPolyFunc' . typeOf

isPolyFunc' :: Type -> Bool
isPolyFunc' (TyForAll _ _) = True
isPolyFunc' _ = False

-- tyVars
-- Returns a list of all tyVars
tyVars :: ASTContainer m Type => m -> [Type]
tyVars = evalASTs tyVars'

tyVars' :: Type -> [Type]
tyVars' t@(TyVar _) = [t]
tyVars' _ = []

-- | hasTyFuns
hasTyFuns :: ASTContainer m Type => m -> Bool
hasTyFuns = getAny . evalASTs hasTyFuns'

hasTyFuns' :: Type -> Any
hasTyFuns' (TyFun _ _) = Any True
hasTyFuns' _ = Any False

-- hasTyBottom
hasTyBottom :: ASTContainer m Type => m -> Bool
hasTyBottom = getAny . evalASTs hasTyBottom'

hasTyBottom' :: Type -> Any
hasTyBottom' TyBottom = Any True
hasTyBottom' _ = Any False

-- | numArgs
numArgs :: Typed t => t -> Int
numArgs = length . argumentTypes

-- | argumentTypes
-- Gives the types of the arguments of the functions
data ArgType = JustType Type | BindType Id

argumentTypes :: Typed t => t -> [Type]
argumentTypes = argumentTypes' . typeOf

argumentTypes' :: Type -> [Type]
argumentTypes' (TyForAll (AnonTyBndr t1) t2) = t1:argumentTypes' t2
argumentTypes' (TyForAll (NamedTyBndr _) t2) = TYPE:argumentTypes' t2
argumentTypes' (TyFun t1 t2) = t1:argumentTypes' t2
argumentTypes' _ = []

spArgumentTypes :: Typed t => t -> [ArgType]
spArgumentTypes = spArgumentTypes' . typeOf

spArgumentTypes' :: Type -> [ArgType]
spArgumentTypes' (TyForAll (AnonTyBndr t1) t2) = JustType t1:spArgumentTypes' t2
spArgumentTypes' (TyForAll (NamedTyBndr i) t2) = BindType i:spArgumentTypes' t2
spArgumentTypes' (TyFun t1 t2) = JustType t1:spArgumentTypes' t2
spArgumentTypes' _ = []

tyForAllBindings :: Typed t => t -> [Id]
tyForAllBindings = tyForAllBindings' . typeOf

tyForAllBindings' :: Type -> [Id]
tyForAllBindings' (TyForAll (NamedTyBndr i) t) = i:tyForAllBindings' t
tyForAllBindings' (TyForAll _ t) = tyForAllBindings' t
tyForAllBindings' (TyFun t t') = tyForAllBindings' t ++ tyForAllBindings t'
tyForAllBindings' _ = []

anonArgumentTypes :: Typed t => t -> [Type]
anonArgumentTypes = anonArgumentTypes' . typeOf

anonArgumentTypes' :: Type -> [Type]
anonArgumentTypes' (TyForAll _ t) = anonArgumentTypes' t
anonArgumentTypes' (TyFun t1 t2) = t1:anonArgumentTypes' t2
anonArgumentTypes' _ = []

-- | returnType
-- Gives the return type if the given function type is fully saturated
returnType :: Typed t => t -> Type
returnType = returnType' . typeOf

returnType' :: Type -> Type
returnType' (TyForAll _ t) = returnType' t
returnType' (TyFun _ t) = returnType' t
returnType' t = t

-- | polyIds
-- Returns all polymorphic Ids in the given type
polyIds :: Type -> [Id]
polyIds = fst . splitTyForAlls

-- | splitTyForAlls
-- Turns TyForAll types into a list of type ids
splitTyForAlls :: Type -> ([Id], Type)
splitTyForAlls (TyForAll (NamedTyBndr i) t) =
    let
        (i', t') = splitTyForAlls t
    in
    (i:i', t')
splitTyForAlls t = ([], t)


-- Turns TyFun types into a list of types
splitTyFuns :: Type -> [Type]
splitTyFuns (TyFun t t') = t:splitTyFuns t'
splitTyFuns t = [t]

-- | tyForAlls
-- Nests a new type in TyForAlls
nestTyForAlls :: Type -> (Type -> Type)
nestTyForAlls (TyForAll b t) = TyForAll b . nestTyForAlls t
nestTyForAlls _ = id

inTyForAlls :: Type -> Type
inTyForAlls (TyForAll _ t) = inTyForAlls t
inTyForAlls t = t

-- | unApp
-- Unravels the application spine.
passedArgs :: Expr -> [Expr]
passedArgs = reverse . passedArgs'

passedArgs' :: Expr -> [Expr]
passedArgs' (App e e') = e':passedArgs' e
passedArgs' _ = []

appCenter :: Expr -> Expr
appCenter (App a _) = appCenter a
appCenter e = e
