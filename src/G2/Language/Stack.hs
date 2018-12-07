{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module G2.Language.Stack
    ( Stack
    , empty
    , push
    , pop
    , popN
    , toList) where

import G2.Language.AST
import G2.Language.Naming
import G2.Language.Syntax

newtype Stack a = Stack [a] deriving (Show, Eq, Read)

-- | Is the `Stack` empty?
empty :: Stack a
empty = Stack []

-- | Push a `Frame` onto the `Stack`.
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

-- | Pop a `Frame` from the `Stack`, should it exist.
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

-- | Pop @n@ frames from the `Stack`, or, if the `Stack` has less than @n@
-- frames, empty the `Stack`.
popN :: Stack a -> Int -> ([a], Stack a)
popN s 0 = ([], s)
popN s n = case pop s of
    Just (x, s') -> 
        let
            (xs, s'') = popN s' (n - 1)
        in
        (x:xs, s'')
    Nothing -> ([], s)

-- | Convert a `Stack` to a list.
toList :: Stack a -> [a]
toList (Stack xs) = xs

instance ASTContainer a Expr => ASTContainer (Stack a) Expr where
    containedASTs (Stack s) = containedASTs s
    modifyContainedASTs f (Stack s) = Stack $ modifyContainedASTs f s

instance ASTContainer a Type => ASTContainer (Stack a) Type where
    containedASTs (Stack s) = containedASTs s
    modifyContainedASTs f (Stack s) = Stack $ modifyContainedASTs f s

instance Named a => Named (Stack a) where
    names (Stack s) = names s
    rename old new (Stack s) = Stack $ rename old new s
    renames hm (Stack s) = Stack $ renames hm s