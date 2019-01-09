{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Defines most of the central language in G2 This language closely resembles Core Haskell.
-- The central datatypes are `Expr` and `Type`.
module G2.Language.Syntax
    ( module G2.Language.Syntax
    , module G2.Language.Internal.Syntax
    ) where

import G2.Language.Internal.Syntax

import Data.Hashable
import qualified Data.Text as T

-- | The native GHC definition states that a `Program` is a list of `Binds`.
-- This is used only in the initial stages of the translation from GHC Core.
-- We quickly shift to using a `State`.
type Program = GProgram T.Text

-- | Binds `Id`s to `Expr`s, primarily in @let@ `Expr`s
type Binds = GBinds T.Text

-- | Records a location in the source code
type Loc = GLoc

instance Hashable GLoc

-- | Records a span in the source code.
--
-- Invariant:
--
-- >  file start == file end
type Span = GSpan

instance Hashable GSpan

-- | A name has three pieces: an occurence name, Maybe a module name, and a Unique Id.
type Name = GName T.Text

-- | Disregards the Span
instance Hashable n => Hashable (GName n) where
    hashWithSalt s (Name n m i _) =
        s `hashWithSalt`
        n `hashWithSalt`
        m `hashWithSalt` i

-- | Pairing of a `Name` with a `Type`
type Id = GId T.Text

instance Hashable n => Hashable (GId n)

-- | Indicates the purpose of the a Lambda binding
type LamUse = GLamUse

instance Hashable GLamUse
 
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
type Expr = GExpr T.Text

instance Hashable n => Hashable (GExpr n)

-- | These are known, and G2-augmented operations, over unwrapped
-- data types such as Int#, Char#, Double#, etc.
-- Generally, calls to these should actually be created using the functions in:
--
--    "GLanguage.Primitives"
--
-- And evaluation over literals can be peformed with the functions in:
--
--     "GExecution.PrimitiveEval" 
type Primitive = GPrimitive

instance Hashable GPrimitive

-- | Literals for denoting unwrapped types such as Int#, Double#.
type Lit = GLit

instance Hashable GLit

-- | Data constructor.
type DataCon = GDataCon T.Text

instance Hashable n => Hashable (GDataCon n)

-- | AltMatches.
type AltMatch = GAltMatch T.Text

instance Hashable n => Hashable (GAltMatch n)

-- | `Alt`s consist of the `AltMatch` that is used to match
-- them, and the `Expr` that is evaluated provided that the `AltMatch`
-- successfully matches.
type Alt = GAlt T.Text

instance Hashable n => Hashable (GAlt n)

-- | Used in the `TyForAll`, to bind an `Id` to a `Type`
type TyBinder = GTyBinder T.Text

instance Hashable n => Hashable (GTyBinder n)

type Coercion = GCoercion T.Text

instance Hashable n => Hashable (GCoercion n)

-- | Types are decomposed as follows:
-- * Type variables correspond to the aliasing of a type
-- * TyLitInt, TyLitFloat etc denote unwrapped primitive types.
-- * Function type. For instance (assume Int): \x -> x + 1 :: TyFun TyInt TyInt
-- * Application, often reducible: (TyApp (TyFun TyInt TyInt) TyInt) :: TyInt
-- * Type constructor (see below) application creates an actual type
-- * For all types
-- * BOTTOM
type Type = GType T.Text

type Kind = Type

instance Hashable n => Hashable (GType n)

type Tickish = GTickish T.Text

instance Hashable n => Hashable (GTickish n)

-- | Represents a function call, with it's arguments and return value as Expr
type FuncCall = GFuncCall T.Text

instance Hashable n => Hashable (GFuncCall n)
