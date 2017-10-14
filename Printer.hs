module Printer where

import Types


import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

instance Pretty Expr where
 pPrint (Leaf i) = pPrint i
 pPrint Fail = text "fail"
 pPrint (Switch o branches Nothing) =
  vcat $ (cat [text "switch ", pPrint o, text " {"]):
    (map (\(o, a) -> nest 2 (cat [pPrint o, text " -> ", pPrint a])) branches) ++
    [text "}"]
 pPrint (Switch o branches (Just def)) =
  vcat $ (cat [text "switch ", pPrint o, text " {"]):
    (map (\(o, a) -> nest 2 (cat [pPrint o, text " -> ", pPrint a])) branches)
    ++ [(cat [text "_", text " -> ", pPrint def])]
    ++ [text "}"]

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
