module Checker where

import Types
import Common
import Data.Maybe
import qualified Data.Map as Map

-- TODO: Need to setify these
getRootConstructors :: PatMat -> [Pat]
getRootConstructors = map head

useful :: Env -> PatMat -> PatVec -> Bool
useful _ q [] = q == []
useful env pmat pv@(q:qs) =
  let checkUseful constr =
        let smat = specializeMatrix constr pmat
            svec = specializeVector constr pv in
        case svec of
          Nothing -> error "foo"
          Just svec -> useful env smat svec
  in
  case isWildcard q of
    False -> checkUseful q
    True ->
      let sigma = getRootConstructors pmat
          isComplete = sigmaComplete env sigma
      in if null isComplete then
           any checkUseful sigma
         else
           let dmat = defaultMatrix pmat in
           useful env dmat qs

exhaustive :: Env -> Match -> Bool
exhaustive env (Match str ps) =
  let check mat pats = case pats of
        [] -> not (useful env mat [PatWild])
        (p:ps) ->
          if useful env mat [p] then check ([p]:mat) ps
          else error ("Dead pattern found " ++ show p)
  in check [] ps
