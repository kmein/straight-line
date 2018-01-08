module Evaluator (expression, run) where

import Types

import Control.Monad.State (State, evalState, gets, modify)
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
import qualified Data.Map.Strict as M (fromList, insert, lookup)

eval :: Program -> State Memory Natural
eval prog = do
    case prog of
        Nil -> gets (deref Output)
        Cons i is -> evalI i >> eval is
  where
    evalI :: Instruction -> State Memory ()
    evalI (Assign v e) = modify $ \memory -> M.insert v (expression memory e) memory

deref :: Variable -> Memory -> Natural
deref v = fromMaybe 0 . M.lookup v

expression :: Memory -> Expression -> Natural
expression _ (Constant n) = n
expression mem (Add v0 v1) = deref v0 mem + deref v1 mem
expression mem (Multiply v0 v1) = deref v0 mem * deref v1 mem

run :: [Natural] -> Program -> Natural
run mem prog = evalState (eval prog) (M.fromList (zip (map Input [0 ..]) mem))
