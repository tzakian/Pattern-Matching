module Printer where

import Types


import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

instance Pretty Expr where
 pPrint (Leaf i) = pPrint i
 pPrint Fail = text "fail"
 pPrint (Switch o branches Nothing) =
   let switch = cat [text "switch ", pPrint o, lbrace] in
   let brnchs = map (\(o, a) -> nest 2 (hcat [text "case ", pPrint o, text ": -> ", pPrint a])) branches in
   vcat $ switch : brnchs ++ [rbrace]
 pPrint (Switch o branches (Just def)) =
   let switch = cat [text "switch ", pPrint o, lbrace] in
   let brnchs = map (\(o, a) -> nest 2 (hcat [text "case ", pPrint o, text ": -> ", pPrint a])) branches in
   vcat $ switch : brnchs ++ [nest 2 (hcat [text "_", text " -> ", pPrint def]), rbrace]

instance Pretty Obj where
  pPrint (Obj s) = text s
  pPrint (L l) = pPrint l

instance Pretty Occurence where
  pPrint (Lam o) = pPrint o
  pPrint (Access o i) = cat [pPrint o, text "@", pPrint i]

instance Pretty Lit where
  pPrint (S s) = text s
  pPrint (I i) = pPrint i
  pPrint (C c) = pPrint c
  pPrint (B b) = pPrint b
