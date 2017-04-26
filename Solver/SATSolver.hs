module Solver.SATSolver ( solve , Result (..) ) where

data Result = UNSAT | SAT [Int] deriving Show

solve :: Int -> [[Int]] -> Result
solve = solveNaive

-------------
-- solveNaive
-------------
solveNaive :: Int -> [[Int]] -> Result
solveNaive n cnf = if b theanswer
                      then SAT (ans theanswer)
                      else UNSAT
  where choose = (\x -> [x,-x]) <$> [1..n]
        guesses = foldr (\a as -> (:) <$> a <*> as) [[]] choose
        answers = map (\guess -> Cand (and $ or . fmap (flip elem guess) <$> cnf) guess) guesses
        theanswer = mconcat answers

data Candidate = Cand { b :: Bool, ans :: [Int] }

instance Monoid Candidate where
  mempty = (Cand False [])
  mappend (Cand True as) _ = (Cand True as)
  mappend _ (Cand True as) = (Cand True as)
  mappend a _ = a
