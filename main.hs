{-
   Read in CNF of the form
     ```
     1 2 3 5 4
     4 2 3 2 -1
     -3 -4 4
     ```
   which correspond to the expressions
   a \/ b \/ c \/ e \/ d /\ d \/ b \/ c \/ b \/ not a /\ not c \/ not d \/ d
-}

import Solver.SATSolver (solve)

main :: IO ()
main = do cnf <- fmap (fmap read) . fmap words . lines <$> getContents
          print $ solve (maximum (fmap maximum cnf)) cnf
