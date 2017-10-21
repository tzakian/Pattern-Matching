module Checker where

import Types
import Common
import Printer
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- TODO: Need to setify these
getRootConstructors :: PatMat -> [Pat]
getRootConstructors m = filter (not . isWildcard) $ map head m

useful :: Env -> PatMat -> PatVec -> Bool
useful _ q [] = q == []
useful env pmat pv@(q:qs) =
  let checkUseful constr =
        let smat = specializeMatrix constr pmat
            svec = specializeVector constr pv in
        case svec of
          Nothing -> error $ "foo "  ++ show constr ++ " " ++ show pv
          Just svec -> useful env smat svec
  in
  case isWildcard q of
    False -> checkUseful q
    True ->
      let sigma = getRootConstructors pmat
          (present, missing) = sigmaComplete env sigma
      in if null missing && (not . null) present then
           any checkUseful sigma
         else
           let dmat = defaultMatrix pmat in
           useful env dmat qs

exhaustive :: Env -> Match -> Bool
exhaustive env (Match str ps) =
  let check mat pats = case pats of
        [] ->
          if useful env mat [PatWild] then
            let Just cex = generateCounterexample env mat 1 in
            error ("Non-exhaustive pattern match found. The following is a counterexample:\n" ++ prettyShow (head cex))
          else True
        (p:ps) ->
          if useful env mat [p] then check ([p]:mat) ps
          else error ("Dead pattern found " ++ show p)
  in check [] ps

generateCounterexample :: Env -> PatMat -> Int -> Maybe PatVec
generateCounterexample _ [] 0 = Just []
generateCounterexample _ _ 0 = Nothing
generateCounterexample env pmat n =
  let sigma = getRootConstructors pmat in
  let (present, missing) = sigmaComplete env sigma in
  if null missing && (not . null) present then
    case find (\c -> isJust $ generateCounterexample env
                          (specializeMatrix c pmat)
                          ((arityOfPat c) + n - 1)) sigma of
      Nothing -> Nothing
      Just constr ->
        let arity = (arityOfPat constr) in
        let specmat = specializeMatrix constr pmat in
        let Just pv = generateCounterexample env specmat (arity + n - 1) in
        let (l, rest) = splitAt arity pv in
        Just $ fillConstr constr l : rest
  else
    case generateCounterexample env (defaultMatrix pmat) (n - 1) of
      Nothing -> Nothing
      Just rest ->
        if null missing then
          Just $ PatWild : rest
        else
          let constr = head missing in
          Just $ fillConstr constr (repeat PatWild) : rest
