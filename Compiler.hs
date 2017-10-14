module Compiler where

import Types
import Common
import Data.List
import qualified Data.Set as Set

swap :: Int -> Int -> [a] -> [a]
swap i j xs = firstpart ++ [xs !! u] ++ secondpart ++ [xs !! l] ++ thirdpart
  where l = if i < j then i else j
        u = if i > j then i else j
        (firstpart, f:frest) = splitAt l xs
        (secondpart, t:thirdpart) = splitAt (u - l - 1) frest

buildOccurences :: [Occurence] -> Int -> [Occurence]
buildOccurences [] _ = error "Invariant violation: encountered empty occurence list"
buildOccurences (o:rest) n =
  [Access o i | i <- [0..n]] ++ rest

-- Find the first column in the first row that has a non-wildcard, and swap that
-- to be the first column.
-- TODO: Add heuristics for better column selection
reorganizePatterns :: ActMat -> ActMat
reorganizePatterns [] = error "Invariant violation: empty matrix encountered"
reorganizePatterns mat@((row, _):_) =
  case findIndex (not . isWildcard) row of
    Nothing -> error "Invariant violation: Unable to find non-wildcard"
    Just index ->
      map (\(pats, act) -> (swap 0 index pats, act)) mat

getHeadConstructors :: ActMat -> [Pat]
getHeadConstructors mat = removeDuplicates $ concatMap getHead mat
  where
    getHead ([], _) = error "Invariant violation: empty pattern encountered"
    getHead ((p:_), _)
      | isWildcard p = []
      | otherwise = [p]

specializeMatrixWActs :: Pat -> ActMat -> ActMat
specializeMatrixWActs p mat = concatMap spec mat
  where
    spec (pv, a) = case specializeVector p pv of
      Nothing -> []
      Just pv' -> [(pv', a)]

defaultMatrixWAct :: ActMat -> ActMat
defaultMatrixWAct mat = concatMap defPat mat
  where
    defPat :: (PatVec, Int) -> [(PatVec, Int)]
    defPat (p:ps, a) | isWildcard p = [(ps, a)]
    defPat _ = []

objOfConstr :: Pat -> Obj
objOfConstr (PatLit l) = L l
objOfConstr (PatObj (Constr s) _) = Obj s
objOfConstr (PatTuple _) = Obj "tuple"
objOfConstr PatWild = error "Invariant violation: encountered wildcard"

compile :: Env -> [Occurence] -> ActMat -> Expr
compile _ _ [] = Fail
compile env occs@(o:rest) mat@((firstRow, fact):_)
  | all isWildcard firstRow = Leaf(fact)
  | otherwise =
    let rootConstrs = getHeadConstructors mat in
    let aks = map (\c -> (objOfConstr c, compileSubmatrix c)) rootConstrs in
    let sigComplete = sigmaComplete env rootConstrs in
    if null sigComplete then
      Switch o aks Nothing
    else
      let dmat = compile env rest (defaultMatrixWAct mat) in
      Switch o aks (Just dmat)
  where
   compileSubmatrix constr =
     let occs' = buildOccurences occs (arityOfPat constr) in
     let specMat = specializeMatrixWActs constr mat in
     compile env occs' specMat

compilePatterns :: Env -> Match -> Expr
compilePatterns env (Match v pats) =
  compile env [Lam (Obj v)] (zipWith (\p i -> ([p], i)) pats [0..])
