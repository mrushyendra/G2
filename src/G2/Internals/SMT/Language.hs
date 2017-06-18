-- | Language
--   Provides a language definition designed to closely resemble the SMTLIB2 language.

{-# LANGUAGE MultiParamTypeClasses #-}

module G2.Internals.SMT.Language ( module G2.Internals.SMT.Language
                                 , Name) where

-- import qualified Control.Monad.State.Strict as Mon

import G2.Internals.Core.Language
import G2.Internals.Core.AST

-- | SMTHeader
-- These define the two kinds of top level calls we give to the SMT solver.
-- An assertion says the given SMTAST is true
-- A sort decl declares a new sort.
data SMTHeader = Assert SMTAST
               | SortDecl [(Name, [Sort])]

-- | SMTAST
-- These coorespond to first order logic, arithmetic operators, and variables, as supported by an SMT Solver
-- Its use should be confined to interactions with G2.Internals.SMT.* 
data SMTAST = (:>=) SMTAST SMTAST
            | (:>) SMTAST SMTAST
            | (:=) SMTAST SMTAST
            | (:<) SMTAST SMTAST
            | (:<=) SMTAST SMTAST

            | (:&&) SMTAST SMTAST
            | (:||) SMTAST SMTAST
            | (:!) SMTAST
            | (:=>) SMTAST SMTAST

            | (:+) SMTAST SMTAST
            | (:-) SMTAST SMTAST -- Subtraction
            | (:*) SMTAST SMTAST
            | (:/) SMTAST SMTAST
            | Neg SMTAST --Unary negation

            | Ite SMTAST SMTAST SMTAST

            | VInt Int
            | VFloat Float
            | VDouble Double
            | VBool Bool
            | V Name Sort

data Sort = SortInt
          | SortFloat
          | SortDouble
          | SortBool
          | Sort Name

-- This data type is used to describe the specific output format required by various solvers
-- By defining these functions, we can automatically convert from the SMTHeader and SMTAST
-- datatypes, to a form understandable by the solver.
data SMTConverter ast out =
    SMTConverter {
          empty :: out
        , merge :: out -> out -> out

        , assert :: ast -> out

        , (.>=) :: ast -> ast -> ast
        , (.>) :: ast -> ast -> ast
        , (.=) :: ast -> ast -> ast
        , (.<) :: ast -> ast -> ast
        , (.<=) :: ast -> ast -> ast

        , (.&&) :: ast -> ast -> ast
        , (.||) :: ast -> ast -> ast
        , (.!) :: ast -> ast
        , (.=>) :: ast -> ast -> ast

        , (.+) :: ast -> ast -> ast
        , (.-) :: ast -> ast -> ast
        , (.*) :: ast -> ast -> ast
        , (./) :: ast -> ast -> ast
        , neg :: ast -> ast

        , ite :: ast -> ast -> ast -> ast

        , int :: Int -> ast
        , float :: Float -> ast
        , double :: Double -> ast
        , bool :: Bool -> ast
        , var :: Name -> ast -> ast

        , sortInt :: ast
        , sortFloat :: ast
        , sortDouble :: ast
        , sortBool :: ast
        , sortName :: Name -> ast
    }

-- converterToMonad1 :: ((SMTConverter ast out) -> ast -> ast) -> ast -> Mon.State (SMTConverter ast out) ast
-- converterToMonad1 f x = do
--     con <- Mon.get
--     return $ f con x

-- converterToMonad2 :: ((SMTConverter ast out) -> ast -> ast -> ast) -> ast -> ast -> Mon.State (SMTConverter ast out) ast
-- converterToMonad2 f x y = do
--     con <- Mon.get
--     return $ f con x y

-- assert' :: ast -> Mon.State (SMTConverter ast out) out
-- assert' x = do
--     con <- Mon.get
--     return $ Mon.put (merge con (assert con x) con)
    

instance AST SMTAST where
    children (x :>= y) = [x, y]
    children (x :> y) = [x, y]
    children (x := y) = [x, y]
    children (x :< y) = [x, y]
    children (x :<= y) = [x, y]

    children (x :&& y) = [x, y]
    children (x :|| y) = [x, y]
    children ((:!) x) = [x]
    children (x :=> y) = [x, y]

    children (x :+ y) = [x, y]
    children (x :- y) = [x, y]
    children (x :* y) = [x, y]
    children (x :/ y) = [x, y]
    children (Neg x) = [x]

    children (Ite x x' x'') = [x, x', x'']

    children _ = []

    modifyChildren f (x :>= y) = f x :>= f y
    modifyChildren f (x :> y) = f x :> f y
    modifyChildren f (x := y) = f x := f y
    modifyChildren f (x :< y) = f x :< f y
    modifyChildren f (x :<= y) = f x :<= f y

    modifyChildren f (x :&& y) = f x :&& f y
    modifyChildren f (x :|| y) = f x :|| f y
    modifyChildren f ((:!) x) = (:!) (f x)
    modifyChildren f (x :=> y) = f x :=> f y

    modifyChildren f (x :+ y) = f x :+ f y
    modifyChildren f (x :- y) = f x :- f y
    modifyChildren f (x :* y) = f x :* f y
    modifyChildren f (x :/ y) = f x :/ f y
    modifyChildren f (Neg x) = Neg (f x)

    modifyChildren f (Ite x x' x'') = Ite (f x) (f x') (f x'')

    modifyChildren _ e = e

instance AST Sort where
    children _ = []
    modifyChildren s = s

instance ASTContainer SMTAST Sort where
    containedASTs (V _ s) = [s]
    containedASTs x = eval containedASTs x

    modifyContainedASTs f (V n s) = V n (modify f s)
    modifyContainedASTs f x = modify (modifyContainedASTs f) x