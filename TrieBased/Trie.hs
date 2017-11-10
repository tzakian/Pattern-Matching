module Trie where

import Types

import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

newtype Trie a = Trie { children :: Map.Map a (Trie a) }
  deriving (Show)

emptyTrie :: Trie PatToken
emptyTrie = Trie { children = Map.empty }

isEnd :: Trie PatToken -> Bool
isEnd t = Map.null (children t)

hasWildcard :: Map.Map PatToken (Trie PatToken) -> Bool
hasWildcard = Map.member PTWild

insert :: Env -> Trie PatToken -> Pattern -> Trie PatToken
-- We prune the trie here
insert env (Trie t) (Pat PTWild Nothing) = Trie $ Map.singleton PTWild emptyTrie
insert env (Trie t) (Pat PTWild (Just rest)) = Trie $ (flip Map.mapWithKey) t $ \k node ->
      let epat = expandPat env k (Just rest) in
      insert env node epat
-- We just insert
insert env (Trie t) (Pat p Nothing)
 | hasWildcard t || Map.member p t = Trie t
 | otherwise = Trie $ Map.insert p emptyTrie t
insert env (Trie t) (Pat p (Just rest)) =
  case Map.lookup p t of
    Just kids -> Trie $ Map.insert p (insert env kids rest) t
    Nothing -> Trie $ Map.insert p (insert env emptyTrie rest) t

expandPat :: Env -> PatToken -> Maybe Pattern -> Pattern
expandPat env pt pat =
  let len = arityOfPatToken env pt in
  case pat of
    Nothing -> makeWildcards len
    Just pat -> generateWildcards len pat

arityOfPatToken :: Env -> PatToken -> Int
arityOfPatToken env pat = case Map.lookup pat env of
  Nothing -> error $ "Unable to find constructor: " ++ show pat
  Just kids -> fromJust $ Map.lookup pat kids

generateWildcards :: Int -> Pattern -> Pattern
generateWildcards 0 continuation = continuation
generateWildcards len continuation =
  Pat PTWild $ Just $ generateWildcards (len - 1) continuation

makeWildcards :: Int -> Pattern
makeWildcards 0 = error "foo"
makeWildcards 1 = patwild
makeWildcards len =
  Pat PTWild $ Just $ makeWildcards (len - 1)

isCompleteLevel :: Env -> Map.Map PatToken (Trie PatToken) -> Bool
isCompleteLevel env children =
  let nonWildcards = Map.filterWithKey (\k _ -> k /= PTWild) children in
  if Map.null nonWildcards then False else
    -- Now grab an example elem
    let (exampleElem, _) = Map.findMin nonWildcards
        -- Grab the constructors that we expect
        sigmaSize = Map.size $ fromJust (Map.lookup exampleElem env)
        -- Grab the constructors that we _have_
        constrSize = Map.size nonWildcards in
    sigmaSize == constrSize

-- | Given a pattern @p, determine if that pattern is in the trie
useful :: Env -> Pattern -> Trie PatToken -> Bool
useful env (Pat PTWild Nothing) (Trie t)
  | Map.null t = True -- _ : emptyTrie
  | hasWildcard t = False -- if we have a wild child. This plays in with truncation in insertion
  | isCompleteLevel env t = not $ Map.null $ (flip Map.filterWithKey) t $ \k node ->
      -- If we have a complete level keep on chugging. Useful, if it's useful for one of its children
      if arityOfPatToken env k == 0 then False else
        let rest' = expandPat env k Nothing in
        useful env rest' node
  | otherwise = True
useful _ (Pat p Nothing) (Trie t) =
  case Map.lookup p t of
    Nothing -> not $ hasWildcard t -- Useful if we haven't seen it before, and if we don't have a wildcard
    Just l  -> not $ isEnd l -- Not useful unless this isn't the end. (??) <-- check!
useful env (Pat p (Just rest)) (Trie children) =
  case p of
    PTConstr str ->
      case Map.lookup p children of
        Nothing ->
          case Map.lookup PTWild children of
            Just kids  -> useful env rest kids -- found a wild child, so recurse
            Nothing -> True -- Didn't find a wild child, so we know at this point it's useful
        Just node -> useful env rest node -- found a child, so recurse
    PTWild ->
       if isCompleteLevel env children
       then
          -- for each pattern token in children generate wildcards of the proper
          -- arity and then go down that constructors path
         not $ Map.null $ (flip Map.filterWithKey) children $ \k node ->
          -- Expand out the pattern so that it has the proper number of wildcards
          -- to match the arity of the constructor of edge that we are traversing.
          let rest' = expandPat env k (Just rest) in
          -- useful iff it is useful for at least one of its children
          useful env rest' node
       else
         let wildcard = Map.filterWithKey (\k _ -> k == PTWild) children in
          case Map.size wildcard of
            0 -> True -- If we don't have a wildcard (and level isn't complete), then we're done
            1 -> useful env rest $ snd $ Map.findMin wildcard -- recurse down the wild child
            _ -> error "Trie invariant violated" -- Set invariant broken

exhaustive :: Env -> Match -> Bool
exhaustive env (Match str ps) =
  let check trie pats = case pats of
        [] ->
          let b = useful env patwild trie in
          if b then False
          else True
        (p:ps) ->
          let pp = convertPat p Nothing in
          let b = useful env pp trie in
          if b then check (insert env trie pp) ps
          else trace (show trie) $
            error ("Dead pattern found " ++ show p)
  in check emptyTrie ps
