-- DO NOT IMPORT THIS MODULE DIRECTLY.  USE:
--
--    "G2.Language.Syntax"
--
-- instead.
-- This is split off to allow it to be imported into a modified GHC.
-- Dependencies that are not allowed by GHC (such as Data.Text) are eliminated.
{-# LANGUAGE DeriveGeneric #-}

module G2.Language.Internal.Syntax where

import GHC.Generics (Generic)

-- | The native GHC definition states that a `Program` is a list of `Binds`.
-- This is used only in the initial stages of the translation from GHC Core.
-- We quickly shift to using a `State`.
type GProgram n = [GBinds n]

-- | Binds `Id`s to `Expr`s, primarily in @let@ `Expr`s
type GBinds n = [(GId n, GExpr n)]

-- | Records a location in the source code
data GLoc = Loc { line :: Int
                , col :: Int
                , file :: String } deriving (Show, Eq, Read, Ord, Generic)

-- | Records a span in the source code.
--
-- Invariant:
--
-- >  file start == file end
data GSpan = Span { start :: GLoc
                  , end :: GLoc } deriving (Show, Eq, Read, Ord, Generic)

-- | A name has three pieces: an occurence name, Maybe a module name, and a Unique Id.
data GName n = Name n (Maybe n) Int (Maybe GSpan) deriving (Show, Read, Generic)

-- | Disregards the Span
instance Eq n => Eq (GName n) where
    Name n m i _ == Name n' m' i' _ = n == n' && m == m' && i == i'

-- | Disregards the Span
instance Ord n => Ord (GName n) where
    Name n m i _ `compare` Name n' m' i' _ = (n, m, i) `compare` (n', m', i')

-- | Pairing of a `Name` with a `Type`
data GId n = Id (GName n) (GType n) deriving (Show, Eq, Read, Generic)

-- | Indicates the purpose of the a Lambda binding
data GLamUse = TermL -- ^ Binds at the term level 
             | TypeL -- ^ Binds at the type level
             deriving (Show, Eq, Read, Generic)

idName :: GId n -> GName n
idName (Id name _) = name
 
{-| This is the main data type for our expression language.

 1. @`Var` `Id`@ is a variable.  Variables may be bound by a `Lam`, `Let`
 or `Case` `Expr`, or be bound in the `ExprEnv`.  A variable may also be
 free (unbound), in which case it is symbolic

 2. @`Lit` `Lit`@ denotes a literal.

 3. @`Data` `DataCon`@ denotes a Data Constructor

 4. @`App` `Expr` `Expr`@ denotes function application.
    For example, the function call:

     @ f x y @
    would be represented as

     @ `App`
       (`App`
         (`Var` (`Id` (`Name` "f" Nothing 0 Nothing) (`TyFun` t (`TyFun` t t))))
         (`Var` (`Id` (`Name` "x" Nothing 0 Nothing) t))
       )
       (`Var` (`Id` (`Name` "y" Nothing 0 Nothing) t)) @

 5. @`Lam` `LamUse` `Id` `Expr`@ denotes a lambda function.
    The `Id` is bound in the `Expr`.
    This binding may be on the type type or term level, depending on the `LamUse`.

 6. @`Case` e i as@ splits into multiple `Alt`s (Alternatives),
    Depending on the value of @e@.  In each Alt, the `Id` @i@ is bound to @e@.
    The `Alt`s must always be exhaustive- there should never be a case where no `Alt`
    can match a given `Expr`.

 7. @`Type` `Type`@ gives a `Expr` level representation of a `Type`.
    These only ever appear as the arguments to polymorphic functions,
    to determine the `Type` bound to type level variables.

 8. @`Cast` e (t1 `:~` t2)@ casts @e@ from the type @t1@ to @t2@
    This requires that @t1@ and @t2@ have the same representation.

 9. @`Coercion` `Coercion`@ allows runtime passing of `Coercion`s to `Cast`s.

 10. @`Tick` `Tickish` `Expr`@ records some extra information into an `Expr`.

 11. @`NonDet` [`Expr`] gives a nondeterministic choice between multiple options
     to continue execution with.

 12. @`SymGen` `Type`@ evaluates to a fresh symbolic variable of the given type.

 13. @`Assume` b e@ takes a boolean typed expression @b@,
     and an expression of arbitrary type @e@.
     During exectuion, @b@ is reduced to SWHNF, and assumed.
     Then, execution continues with @b@.

 14. @`Assert` fc b e@ is similar to `Assume`, but asserts the @b@ holds.
     The `Maybe` `FuncCall` allows us to optionally indicate that the
     assertion is related to a specific function. -}
data GExpr n = Var (GId n)
             | Lit GLit
             | Prim GPrimitive (GType n)
             | Data (GDataCon n)
             | App (GExpr n) (GExpr n)
             | Lam GLamUse (GId n) (GExpr n)
             | Let (GBinds n) (GExpr n)
             | Case (GExpr n) (GId n) [GAlt n]
             | Type (GType n)
             | Cast (GExpr n) (GCoercion n)
             | Coercion (GCoercion n)
             | Tick (GTickish n) (GExpr n)
             | NonDet [GExpr n]
             | SymGen (GType n)
             | Assume (Maybe (GFuncCall n)) (GExpr n) (GExpr n)
             | Assert (Maybe (GFuncCall n)) (GExpr n) (GExpr n)
             deriving (Show, Eq, Read, Generic)

-- | These are known, and G2-augmented operations, over unwrapped
-- data types such as Int#, Char#, Double#, etc.
-- Generally, calls to these should actually be created using the functions in:
--
--    "G2.Language.Primitives"
--
-- And evaluation over literals can be peformed with the functions in:
--
--     "G2.Execution.PrimitiveEval" 
data GPrimitive = Ge
                | Gt
                | Eq
                | Neq
                | Lt
                | Le
                | And
                | Or
                | Not
                | Implies
                | Iff
                | Plus
                | Minus
                | Mult
                | Div
                | DivInt
                | Quot
                | Mod
                | Negate
                | SqRt
                | IntToFloat
                | IntToDouble
                | FromInteger
                | ToInteger
                | ToInt
                | Error
                | Undefined
                | BindFunc
                deriving (Show, Eq, Read, Generic)

-- | Literals for denoting unwrapped types such as Int#, Double#.
data GLit = LitInt Integer
          | LitFloat Rational
          | LitDouble Rational
          | LitChar Char
          | LitString String
          | LitInteger Integer
          deriving (Show, Eq, Read, Generic)

-- | Data constructor.
data GDataCon n = DataCon (GName n) (GType n) deriving (Show, Eq, Read, Generic)

-- | AltMatches.
data GAltMatch n = DataAlt (GDataCon n) [GId n] -- ^ Match a datacon. The number of `Id`s
                                                -- must match the number of term arguments
                                                -- for the datacon.
                 | LitAlt GLit
                 | Default
                 deriving (Show, Eq, Read, Generic)

-- | `Alt`s consist of the `AltMatch` that is used to match
-- them, and the `Expr` that is evaluated provided that the `AltMatch`
-- successfully matches.
data GAlt n = Alt (GAltMatch n) (GExpr n) deriving (Show, Eq, Read, Generic)

altMatch :: GAlt n -> GAltMatch n
altMatch (Alt am _) = am

-- | Used in the `TyForAll`, to bind an `Id` to a `Type`
data GTyBinder n = AnonTyBndr (GType n)
                 | NamedTyBndr (GId n)
                 deriving (Show, Eq, Read, Generic)

data GCoercion n = GType n :~ GType n deriving (Eq, Show, Read, Generic)

-- | Types are decomposed as follows:
-- * Type variables correspond to the aliasing of a type
-- * TyLitInt, TyLitFloat etc denote unwrapped primitive types.
-- * Function type. For instance (assume Int): \x -> x + 1 :: TyFun TyInt TyInt
-- * Application, often reducible: (TyApp (TyFun TyInt TyInt) TyInt) :: TyInt
-- * Type constructor (see below) application creates an actual type
-- * For all types
-- * BOTTOM
data GType n = TyVar (GId n)
             | TyLitInt 
             | TyLitFloat 
             | TyLitDouble
             | TyLitChar 
             | TyLitString
             | TyFun (GType n) (GType n)
             | TyApp (GType n) (GType n)
             | TyCon (GName n) (GKind n)
             | TyForAll (GTyBinder n) (GType n)
             | TyBottom
             | TYPE
             | TyUnknown
             deriving (Show, Eq, Read, Generic)

type GKind n = GType n

data GTickish n = Breakpoint GSpan -- ^ A breakpoint for the GHC Debugger
                | NamedLoc (GName n) -- ^ A G2 specific tick, intended to allow,
                                     -- in concert with a @`Reducer`@, for domain
                                     -- specific modifications to a
                                     -- @`State`@'s tracking field.
                deriving (Show, Eq, Read, Generic)


-- | Represents a function call, with it's arguments and return value as Expr
data GFuncCall n = FuncCall { funcName :: GName n
                            , arguments :: [GExpr n]
                            , returns :: GExpr n } deriving (Show, Eq, Read, Generic)
