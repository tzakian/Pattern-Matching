module Types where
import qualified Data.Map as Map
import qualified Data.Set as Set

data Pat =
  PatLit Lit
  | PatObj Constr (Maybe [Pat])
  | PatTuple [Pat]
  | PatWild
  deriving (Show, Eq, Ord)

data Lit = S String | I Int | C Char | B Bool
  deriving (Show, Eq, Ord)

data Constr = Constr String
  deriving (Show, Eq, Ord)

data Match = Match String [Pat]
  deriving (Show, Eq, Ord)

type PatVec = [Pat]
type PatMat = [PatVec]

type Env = Map.Map String (Map.Map String Int)

------ Language we compile down to
data Obj = Obj String | L Lit
  deriving (Show, Eq, Ord)

data Occurence = Lam Obj | Access Occurence Int
  deriving (Show, Eq, Ord)

data Expr = Leaf Int
 | Fail
 | Switch Occurence [(Obj, Expr)] (Maybe Expr)
 deriving (Show, Eq, Ord)

data PMatch = PMat String [(Pat, Int)]
  deriving (Show, Eq, Ord)

type ActMat = [(PatVec, Int)]
