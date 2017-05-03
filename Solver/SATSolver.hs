module Solver.SATSolver
  ( solve
  , solveNaive
  , solveDPLL
  , Result (..)
  ) where

import qualified Data.Set as S
import           Control.Arrow (first, second)
import           Control.Monad
import           Data.Monoid
import           Debug.Trace

data Result = UNKNOWN | UNSAT | SAT [Int] deriving Show

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
  deriving (Show)

solveDPLL = solveDPLL' . split

split :: [[Int]] -> ClauseSet Int
split cnf = propagate (setjoin pures) (ClauseSet S.empty clausez)
  where (pures, clausez) = S.partition ((==1).length) (S.fromList $ S.fromList <$> cnf)

setjoin :: Ord a => S.Set (S.Set a) -> S.Set a
setjoin = S.fold S.union S.empty

checkLits :: ClauseSet Int -> Result
checkLits cnf = if S.null (clauses cnf) 
                   then if consistent
                           then SAT $ S.toList flat
                           else UNSAT
                   else UNKNOWN
   where flat = lits cnf
         consistent = S.null $ uncurry S.intersection $ second (S.map abs) (S.partition (>0) flat)

solveDPLL' :: ClauseSet Int -> Result
solveDPLL' cnf
  | SAT model <- checkLits cnf
  = SAT model
  | UNSAT <- checkLits cnf
  = UNSAT
  | hasEmpty cnf
  = UNSAT
  | SAT model <- checkLits newcnf
  = SAT model
  | UNSAT <- checkLits newcnf
  = UNSAT
  | hasEmpty newcnf
  = UNSAT
  | otherwise
  = solveDPLL' (propagate (S.singleton lit) newcnf) <>
    solveDPLL' (propagate (S.singleton (negate lit)) newcnf)
  where newcnf = traceShowId $ propagate (S.union (units cnf) (pures cnf)) cnf
        lit = somelit newcnf
        somelit = head . S.toList . head . S.toList . clauses

        units = setjoin . S.filter ((==1).length) . clauses

hasEmpty c = or $ S.map S.null (clauses c)
pures cnf = uncurry S.difference (S.partition (>0) (setjoin $ clauses cnf))

propagate :: S.Set Int -> ClauseSet Int -> ClauseSet Int
propagate units cnf = foldr propagateOne cnf units
  where
  propagateOne :: Int -> ClauseSet Int -> ClauseSet Int
  propagateOne unit (ClauseSet ls cs)
    = ClauseSet (S.insert unit ls)
                (S.map (S.delete (negate unit)) (S.filter (not . S.member unit) cs))

-------------
-- solveNaive
-------------
solveNaive :: Int -> [[Int]] -> Result
solveNaive n cnf = mconcat answers
  where choose = (\x -> [x,-x]) <$> [1..n]
        guesses = foldr (\a as -> (:) <$> a <*> as) [[]] choose
        answers = map (\guess -> result (and $ or . fmap (flip elem guess) <$> cnf) guess) guesses
