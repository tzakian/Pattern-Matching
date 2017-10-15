module Common where

import Types
import Data.Maybe
import qualified Data.Map as Map

removeDuplicates :: [Pat] -> [Pat]
removeDuplicates pats = snd $
  foldl (\(seen, acc) x -> if (canonicalize x) `elem` seen
                    then (seen, acc)
                    else (seen ++ [canonicalize x], x:acc)) ([],[]) pats
  where
    canonicalize (PatTuple _)        = PatTuple []
    canonicalize (PatObj s Nothing)  = (PatObj s Nothing)
    canonicalize (PatObj s (Just _)) = (PatObj s (Just []))
    canonicalize x                   = x

isWildcard :: Pat -> Bool
isWildcard PatWild = True
isWildcard _       = False

arityOfPat :: Pat -> Int
arityOfPat pat = case pat of
  PatLit _ -> 0
  PatWild  -> 0
  PatTuple p -> length p
  PatObj (Constr str) (Just p) -> length p
  PatObj (Constr str) Nothing -> 0

specializeMatrix :: Pat -> PatMat -> PatMat
specializeMatrix pat = catMaybes . map (specializeVector pat)

specializeVector :: Pat -> PatVec -> Maybe PatVec
specializeVector pat (p:ps) = case (pat, p) of
  (PatLit l1, PatLit l2) -> if l1 == l2 then Just ps else Nothing
  (PatTuple _, PatTuple l) -> Just $ l ++ ps
  (PatObj (Constr s1) _, PatObj (Constr s2) Nothing) ->
    if s1 == s2 then Just ps else Nothing
  (PatObj (Constr s1) _, PatObj (Constr s2) (Just p)) ->
    if s1 == s2 then Just (p ++ ps) else Nothing
  (c1, c2) ->
    if (not (isWildcard c1) && (isWildcard c2)) then
     let arity = arityOfPat c1
         wildcards = replicate arity PatWild
     in Just (wildcards ++ ps)
    else Nothing

defaultMatrix :: PatMat -> PatMat
defaultMatrix mat = concatMap defPat mat
  where
    defPat :: PatVec -> [PatVec]
    defPat (p:ps) | isWildcard p = [ps]
    defPat _                     = []

isCompleteLiteral :: Env -> [Pat] -> Lit -> [Pat]
isCompleteLiteral env present lit =
  case lit of
    S _ -> [PatLit (S "a")]
    I _ -> [PatLit (I 0)]
    C _ -> [PatLit (C 'a')]
    B b ->
      let hasOther p = case p of
            PatLit (B b') -> b /= b'
            _             -> False
      in if any hasOther present then []
          else [PatLit (B (not b))]

isCompleteDataSig :: Env -> [Pat] -> String -> [Pat]
isCompleteDataSig env present constr =
  case Map.lookup constr env of
    Nothing -> error ("Unable to find constructor " ++ constr)
    Just rest ->
      let remove acc c _f = Map.delete c acc in
      let left            = (Map.foldlWithKey remove Map.empty rest :: Map.Map String Int) in
      if Map.null left then []
      else [PatObj (Constr (fst (Map.findMin left))) (Just [PatWild])]

sigmaComplete :: Env -> [Pat] -> ([Pat], [Pat])
sigmaComplete env sigma =
  let present = filter (not . isWildcard) sigma in
  case present of
    [] -> (present, [PatWild])
    cons:_ -> case cons of
      PatLit l              -> (present, isCompleteLiteral env present l)
      PatTuple _            -> (present, [])
      PatObj (Constr str) _ -> (present, isCompleteDataSig env present str)

t = Match "f" [PatLit (B True), PatLit (B False)]
tt = Match "f" [PatWild]

e1 :: Map.Map String Int
e1 = Map.fromList [("Nil", 0), ("Cons", 2)]
e2 = Map.fromList [("Nil", e1), ("Cons", e1)]
t4 = Match "f" [(PatObj (Constr "Nil") Nothing), (PatObj (Constr "Cons") (Just [PatWild, PatWild]))]
t5 = Match "f" [(PatObj (Constr "Nil") Nothing), (PatObj (Constr "Cons") (Just [PatLit (I 1), PatWild]))]

p5 = [(PatObj (Constr "Nil") Nothing), (PatObj (Constr "Cons") (Just [PatLit (I 1), PatWild])), PatWild]
p6 = [(PatObj (Constr "Nil") Nothing), (PatObj (Constr "Cons") (Just p5)), PatWild]
t6 = Match "f" p5
t7 = Match "f" p6
