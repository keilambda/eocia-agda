{-# OPTIONS --guardedness #-}

module IR.LInt where

open import IO
open import Data.Bool
open import Data.Integer

mutual
  data Op : Set where
    add : Exp → Exp → Op
    sub : Exp → Exp → Op
    negate : Exp → Op

  data Exp : Set where
    int : ℤ → Exp
    op : Op → Exp

leaf? : Exp → Bool
leaf? (int _) = true
leaf? (op _) = false

exp? : Exp → Bool
exp? (int _) = true
exp? (op (add lhs rhs)) = exp? lhs ∧ exp? rhs
exp? (op (sub lhs rhs)) = exp? lhs ∧ exp? rhs
exp? (op (negate e)) = exp? e

interpret : Exp → IO ℤ
interpret (int n) = pure n
interpret (op (add lhs rhs)) = _+_ <$> interpret lhs <*> interpret rhs
interpret (op (sub lhs rhs)) = _-_ <$> interpret lhs <*> interpret rhs
interpret (op (negate e)) = -_ <$> interpret e

peAdd : Exp → Exp → Exp
peAdd (int n) (int m) = int (n + m)
peAdd lhs rhs = op (add lhs rhs)

peSub : Exp → Exp → Exp
peSub (int n) (int m) = int (n - m)
peSub lhs rhs = op (sub lhs rhs)

peNegate : Exp → Exp
peNegate (int n) = int (- n)
peNegate e = op (negate e)

evaluate : Exp → Exp
evaluate i@(int _) = i
evaluate (op (add lhs rhs)) = peAdd lhs rhs
evaluate (op (sub lhs rhs)) = peSub lhs rhs
evaluate (op (negate e)) = peNegate e
