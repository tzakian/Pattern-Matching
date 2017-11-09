module Trie where

import Types

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

data Trie a = Trie { children :: Map.Map a (Trie a) }
  deriving (Show)

emptyTrie :: Trie PatToken
emptyTrie = Trie { children = Map.empty }

-- TODO: This could probably be combined with useful so that we insert as
-- we go down. But for now we don't do that
insert :: Pattern -> Trie PatToken -> Trie PatToken
insert pat trie = undefined

isEnd :: Trie PatToken -> Bool
isEnd t = Map.null (children t)

expandPat :: Env -> PatToken -> Pattern -> Pattern
expandPat env pt pat =
  let len = arityOfPatToken env pt in
  generateWildcards len pat

arityOfPatToken :: Env -> PatToken -> Int
arityOfPatToken env pat = case Map.lookup pat env of
  Nothing -> error $ "Unable to find constructor: " ++ show pat
  Just kids -> Map.size kids

generateWildcards :: Int -> Pattern -> Pattern
generateWildcards 0 continuation = continuation
generateWildcards len continuation =
  Pat PTWild $ Just $ generateWildcards (len - 1) continuation

isCompleteLevel :: Env -> Map.Map PatToken (Trie PatToken) -> Bool
isCompleteLevel env children =
  let nonWildcards = Map.filterWithKey (\k _ -> k /= PTWild) children in
  if Map.null nonWildcards then False else
    -- Now grab an example elem
    let (exampleElem, _) = Map.findMin nonWildcards
        -- Grab the constructors that we expect
        sigma = Map.keysSet $ fromJust (Map.lookup exampleElem env)
        -- Grab the constructors that we _have_
        constrs = Map.keysSet nonWildcards
        -- Now take their difference
        diff = Set.difference sigma constrs in
    -- Their difference had better be zero
    Set.size diff == 0

-- | Given a pattern @p, determine if that pattern is in the trie
useful :: Env -> Pattern -> Trie PatToken -> Bool
useful env (Pat p Nothing) t = isEnd t
useful env (Pat p (Just rest)) (Trie children) =
  case p of
    PTConstr str ->
      case Map.lookup p children of
        Nothing -> True
        Just node -> useful env rest node
    PTWild ->
      if isCompleteLevel env children
      then
       -- for each pattern token in children generate wildcards of the proper
       -- arity and then go down that constructors path
        not $ Map.null $ (flip Map.filterWithKey) children $ \k node ->
        -- Expand out the pattern so that it has the proper number of wildcards
        -- to match the arity of the constructor of edge that we are traversing.
         let rest' = expandPat env k rest in
         useful env rest' node
      else
        let wildcard = Map.filterWithKey (\k _ -> k == PTWild) children in
         case Map.size wildcard of
           0 -> True
           1 -> useful env rest (snd $ Map.findMin wildcard)
           _ -> error "Core invariant broken: Trie structure violated"

exhaustive :: Env -> Match -> Bool
exhaustive env (Match str ps) =
  let check trie pats = case pats of
        [] ->
          if useful env patwild trie then False
            {-let Just cex = generateCounterexample env mat 1 in-}
            {-error ("Non-exhaustive pattern match found. The following is a counterexample:\n" ++ prettyShow (head cex))-}
          else True
        (p:ps) ->
          let pp = convertPat p Nothing in
          if useful env pp trie then check (insert pp trie) ps
          else error ("Dead pattern found " ++ show p)
  in check emptyTrie ps

-- -- | Given a pattern @p traverse in our pattern trie to the node that finishes
-- -- that patter in the trie.
-- traversePrefix :: Pattern -> Trie PatToken -> Trie PatToken
-- traversePrefix (Pat _ Nothing) t = t
-- traversePrefix (Pat p (Just pat)) t =
--   let Just t' = Map.lookup p (children t) in
--   traversePrefix pat t'
