module Main where

import Control.Monad        (guard)
import Data.Foldable        (foldrM, traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Map.Strict      qualified as L

import Data.Propagator

data Term c f v
  = Constant c
  | Variable v
  | Function f [Term c f v]
  deriving (Eq, Ord, Show)

merge :: (Eq c, Eq f, Ord v) => Term c f v -> Term c f v -> Change (Term c f v)
merge t@(Constant c)    (Constant x) | c == x = Unchanged t
merge t@(Variable v)    (Variable x) | v <= x = Unchanged t
merge   (Variable _)    t                     = Changed t
merge   t               (Variable _)          = Unchanged t
merge   (Function f ts) (Function x xs)
        | f == x && length ts == length xs    = Function f <$> sequence (zipWith merge ts xs)
merge _                 _                     = Incompatible

project :: Eq f => f -> Int -> Term c f v -> Maybe (Term c f v)
project f i term = do
  Function g ts <- pure term
  guard (f == g)
  pure (ts !! i)

rebuild :: v -> f -> Int -> Int -> Term c f v -> Term c f v
rebuild filler f i n term =
  Function f (generate 0)
    where
      wildcard = Variable filler
      generate j
        | j >= n    = []
        | j == i    = term : generate (j+1)
        | otherwise = wildcard : generate (j+1)

link :: v -> f -> Int -> [CellKey] -> CellKey -> Propagator (Term c f v) ()
link filler f n sources target =
  traverse_ con (zip [0..] sources)
    where
      con (i,s) = connect_ Idle s target (Just . rebuild filler f i n)

reverseLink :: Eq f => f -> CellKey -> [CellKey] -> Propagator (Term c f v) ()
reverseLink n source targets =
  traverse_ con (zip [0..] targets)
    where
      con (i,t) = connect_ Idle source t (project n i)

cellify
  :: (Eq c, Eq f, Ord v)
  => v
  -> Term c f v
  -> L.Map v CellKey
  -> Propagator (Term c f v) (Term c f v, CellKey, L.Map v CellKey)
cellify filler term vars =
  case term of
    Constant _ -> do
      key <- cell term merge
      pure (term, key, vars)
    Variable v ->
      getCompose $ L.alterF getOrUpdate v vars
    Function f ts -> do
      -- could optimize, compound ground terms need no cell
      (ts', cells, vars') <- cellifyAll ts vars
      let term' = Function f ts'
      root <- cell term' merge
      link filler f (length ts) cells root
      reverseLink f root cells
      pure (term', root, vars') 
  where
    getOrUpdate maybeKey =
      Compose $
        case maybeKey of
          c@(Just key) -> do
            term' <- readCell key
            pure (term', key, c)
          Nothing -> do
            key <- cell term merge
            pure (term, key, Just key)
    cellifyAll []     vs = pure ([], [], vs)
    cellifyAll (t:ts) vs = do
      (t', key, vs') <- cellify filler t vs
      (ts', keys, vs'') <- cellifyAll ts vs'
      pure (t':ts', key:keys, vs'')

-- Some concrete terms.
data Const
  = ConstInt Int
  | ConstName String
  deriving (Eq, Ord, Show)

int :: Int -> Term Const f v
int = Constant . ConstInt

name :: String -> Term Const f v
name = Constant . ConstName

var :: String -> Term c f String
var = Variable

type SomeTerm = Term Const String String

data Unification = Unification SomeTerm SomeTerm
  deriving (Eq, Ord, Show)

buildNetwork :: Unification -> L.Map String CellKey -> Propagator SomeTerm (L.Map String CellKey)
buildNetwork (Unification t1 t2) vars = do
  (_, k1, vars') <- cellify "_" t1 vars
  (_, k2, vars'') <- cellify "_" t2 vars'
  sync_ Live k1 k2
  pure vars''

unify :: [Unification] -> Either (Error SomeTerm) (L.Map String SomeTerm)
unify eqs =
  fmap snd $
    flip runPropagator empty $ do
      vars <- foldrM buildNetwork L.empty eqs
      traverse readCell vars

program :: [Unification]
program =
  [ Unification (Function "f" [int 1, var "Y"]) (var "X")
  , Unification (var "Z") (Function "g" [name "a", name "b"])
  , Unification (var "Y") (var "Z")
  ]

main :: IO ()
main =
  putStrLn $ show (unify program)