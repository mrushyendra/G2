{-# LANGUAGE OverloadedStrings #-}

module G2.Initialization.KnownValues (initKnownValues) where

import qualified G2.Language.ExprEnv as E
import G2.Language.KnownValues
import G2.Language.Syntax
import G2.Language.TypeClasses
import G2.Language.TypeEnv
import G2.Language.Typing (PresType (..), tyAppCenter, returnType)

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T

initKnownValues :: E.ExprEnv -> TypeEnv -> TypeClasses -> KnownValues
initKnownValues eenv tenv tc =
  let
    numT = typeWithStrName tenv "Num"
    integralT = typeWithStrName tenv "Integral"
    realT = typeWithStrName tenv "Real"
  in
  KnownValues {
      tyInt = typeWithStrName tenv "Int"
    , dcInt = dcWithStrName tenv "Int" "I#"

    , tyFloat = typeWithStrName tenv "Float"
    , dcFloat = dcWithStrName tenv "Float" "F#"

    , tyDouble = typeWithStrName tenv "Double"
    , dcDouble = dcWithStrName tenv "Double" "D#"

    , tyInteger = typeWithStrName tenv "Integer"
    , dcInteger = dcWithStrName tenv "Integer" "Z#"

    , tyChar = typeWithStrName tenv "Char"
    , dcChar = dcWithStrName tenv "Char" "C#"

    , tyBool = typeWithStrName tenv "Bool"
    , dcTrue = dcWithStrName tenv "Bool" "True"
    , dcFalse = dcWithStrName tenv "Bool" "False"

    , tyList = typeWithStrName tenv "[]"
    , dcCons = dcWithStrName tenv "[]" ":"
    , dcEmpty = dcWithStrName tenv "[]" "[]"

    , eqTC = typeWithStrName tenv "Eq"
    , numTC = numT
    , ordTC = typeWithStrName tenv "Ord"
    , integralTC = integralT
    , realTC = realT
    , fractionalTC = typeWithStrName tenv "Fractional"

    , integralExtactReal = superClassExtractor tc integralT realT
    , realExtractNum = superClassExtractor tc realT numT

    , eqFunc = exprWithStrName eenv "=="
    , neqFunc = exprWithStrName eenv "/="

    , plusFunc = exprWithStrName eenv "+"
    , minusFunc = exprWithStrName eenv "-"
    , timesFunc = exprWithStrName eenv "*"
    , divFunc = exprWithStrName eenv "/"
    , negateFunc = exprWithStrName eenv "negate"
    , modFunc = exprWithStrName eenv "mod"

    , fromIntegerFunc = exprWithStrName eenv "fromInteger"
    , toIntegerFunc = exprWithStrName eenv "toInteger"

    , toRatioFunc = exprWithStrName eenv "%"
    , fromRationalFunc = exprWithStrName eenv "fromRational"

    , geFunc = exprWithStrName eenv ">="
    , gtFunc = exprWithStrName eenv ">"
    , ltFunc = exprWithStrName eenv "<"
    , leFunc = exprWithStrName eenv "<="

    , structEqTC = Name "NotDefinedYet" Nothing 0 Nothing
    , structEqFunc = Name "NotDefinedYet" Nothing 0 Nothing

    , andFunc = exprWithStrName eenv "&&"
    , orFunc = exprWithStrName eenv "||"
    , notFunc = exprWithStrName eenv "not"

    , patErrorFunc = exprWithStrName eenv "patError"
    }

exprWithStrName :: E.ExprEnv -> T.Text -> Name
exprWithStrName eenv s =
  case filter (\(Name n _ _ _) -> n == s) $ E.keys eenv of
    n:_ -> n
    _ -> error $ "No expr found in exprWithStrName " ++ (show $ T.unpack s)

typeWithStrName :: TypeEnv -> T.Text -> Name
typeWithStrName tenv s =
  case M.toList $ M.filterWithKey (\(Name n _ _ _) _ -> n == s) tenv of
    (n, _):_ -> n
    _ -> error $ "No type found in typeWithStrName " ++ (show $ T.unpack s)

dcWithStrName :: TypeEnv -> T.Text -> T.Text -> Name
dcWithStrName tenv ts dcs =
  case concatMap dataCon . M.elems $ M.filterWithKey (\(Name n _ _ _) _ -> n == ts) tenv of
    [] -> error $ "No type found in typeWithStrName [" ++
                  (show $ T.unpack ts) ++ "] [" ++ (show $ T.unpack dcs) ++ "]"
    dc -> dcWithStrName' dc dcs

dcWithStrName' :: [DataCon] -> T.Text -> Name
dcWithStrName' (DataCon n@(Name n' _ _ _) _:xs) s =
  if n' == s then n else dcWithStrName' xs s
dcWithStrName' _ s = error $ "No dc found in dcWithStrName [" ++ (show $ T.unpack s) ++ "]"

superClassExtractor :: TypeClasses -> Name -> Name -> Name
superClassExtractor tc tc_n sc_n =
    case lookupTCClass tc_n tc of
        Just c
            | Just (_, i) <- find extractsSC (superclasses c) -> idName i
            | otherwise -> error $ "superClassExtractor: Extractor not found " ++ show (superclasses c)
        Nothing -> error $ "superClassExtractor: Class not found " ++ show tc_n
    where
        extractsSC (t, _) =
            let
                t_c = tyAppCenter . returnType . PresType $ t
            in
            case t_c of
                TyCon n _ -> n == sc_n
                _ -> False
