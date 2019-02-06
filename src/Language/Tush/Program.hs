{-# LANGUAGE NoImplicitPrelude #-}

module Language.Tush.Program where

import Unbound.Generics.LocallyNameless

import ClassyPrelude

import Language.Tush.Types
import Language.Tush.Parse
import Language.Tush.Pretty
import Language.Tush.Eval

programToExp :: Program -> Exp
programToExp (Program defs)
  = let binds = (\(ValDef x) -> x) <$> defs
    in
      Let $ bind (rec binds) (Var (V (string2Name "main") Prefix))

weirdProgram = fmap (eval . programToExp) <$> (parseTush programP $ pack "fact = \\n -> if builtin ieql n 1 then 1 else builtin imul n (fact (builtin isub n 1))\n\nx = 5\n\nmain = fact x\n")
