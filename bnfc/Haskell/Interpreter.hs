module Interpreter where

import AbsWhile
import qualified Data.Map.Strict as Map
import Data.Maybe(fromJust)

execProgram :: Com -> State
execProgram = exec Map.empty

type State = Map.Map String Val
type Val = Integer

aeval :: State -> AExp -> Val
aeval s e = case e of
             ANum n -> n
             AVar (Ident x) -> fromJust (Map.lookup x s)
             APlus  e1 e2 -> aeval s e1 + aeval s e2
             AMinus e1 e2 -> aeval s e1 - aeval s e2
             ATimes e1 e2 -> aeval s e1 * aeval s e2

beval :: State -> BExp -> Bool
beval s b = case b of
              BTrue -> True
              BFalse -> False
              BEq e1 e2 -> aeval s e1 == aeval s e2
              BLe e1 e2 -> aeval s e1 <= aeval s e2
              BNot b' -> not (beval s b')

              BAnd b1 b2 -> beval s b1 && beval s b2
              BOr  b1 b2 -> beval s b1 || beval s b2

exec :: State -> Com -> State
exec s com = case com of
               CAsn (Ident x) e -> Map.insert x (aeval s e) s
               CSeq c1 c2 -> exec (exec s c1) c2
               CLoop b' c -> if beval s b'
                               then exec (exec s c) com
                               else s
