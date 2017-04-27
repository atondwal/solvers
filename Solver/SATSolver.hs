module Solver.SATSolver
  ( solve
  , solveNaive
  , solveDPLL
  , Result (..)
  ) where

import qualified Data.Set as S
import           Control.Arrow (first)
import           Control.Monad
import           Data.Monoid

data Result = UNSAT | SAT [Int] deriving Show

result :: Bool -> [Int] -> Result
result False _ = UNSAT
result True i = SAT i

instance Monoid Result where
  mempty = UNSAT
  mappend (SAT as) _ = SAT as
  mappend _ (SAT as) = SAT as
  mappend a _ = a

solve :: Int -> [[Int]] -> Result
solve _ = solveDPLL

data ClauseSet a = ClauseSet { lits :: S.Set a, clauses :: S.Set (S.Set a) }

solveDPLL = solveDPLL' . split

split :: [[Int]] -> ClauseSet Int
split cnf = uncurry ClauseSet $ first join $ S.partition ((==1).length) (S.fromList $ S.fromList <$> cnf)

checkLits :: ClauseSet Int -> Result
checkLits cnf = if S.null (clauses cnf) && consistent
                   then SAT $ S.toList flat
                   else UNSAT
   where flat = lits cnf
         consistent = S.null $ uncurry S.intersection (S.partition (>0) flat)

solveDPLL' :: ClauseSet Int -> Result
solveDPLL' cnf | SAT model <- checkLits cnf = SAT model
solveDPLL' cnf | hasEmpty cnf = UNSAT
  where hasEmpty c = or $ S.null <$> clauses c
solveDPLL' cnf = solveDPLL' (S.insert lit newcnf) <> solveDPLL' (S.insert (negate lit) newcnf)
  where newcnf = propagate (S.union (units cnf) (pures cnf)) cnf
        lit = somelit newcnf
        somelit = head . S.toList . head . S.toList . clauses

        units = join . S.filter ((==1).length) . clauses

pures cnf = uncurry S.difference (S.partition (>0) (join $ clauses cnf))

propagate :: S.Set Int -> ClauseSet Int -> ClauseSet Int
propagate units cnf = foldr propagateOne cnf units
  where propagateOne :: Int -> ClauseSet Int -> ClauseSet Int
        propagateOne unit (ClauseSet ls cs) = ClauseSet (S.insert unit ls)
                                                        (S.delete (negate unit) . S.delete unit <$> cs)

-- 
-------------
-- solveNaive
-------------
solveNaive :: Int -> [[Int]] -> Result
solveNaive n cnf = mconcat answers
  where choose = (\x -> [x,-x]) <$> [1..n]
        guesses = foldr (\a as -> (:) <$> a <*> as) [[]] choose
        answers = map (\guess -> result (and $ or . fmap (flip elem guess) <$> cnf) guess) guesses
