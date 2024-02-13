module Common.String where

open import Data.Char
open import Data.List
open import Data.Maybe
open import Data.Integer
open import Data.Integer.Show
open import Data.String
open import IO.Primitive

parseInt : String → Maybe ℤ
parseInt s =
  then? (unwrap (parseDigits s)) (λ s' →
  just (digitsToℤ s'))
  where
    parseDigits : String → List (Maybe ℤ)
    parseDigits s = Data.List.map toDigit (toList s) where
      toDigit : Char → Maybe ℤ
      toDigit '0' = just (+ 0)
      toDigit '1' = just (+ 1)
      toDigit '2' = just (+ 2)
      toDigit '3' = just (+ 3)
      toDigit '4' = just (+ 4)
      toDigit '5' = just (+ 5)
      toDigit '6' = just (+ 6)
      toDigit '7' = just (+ 7)
      toDigit '8' = just (+ 8)
      toDigit '9' = just (+ 9)
      toDigit _   = nothing

    unwrap : List (Maybe ℤ) → Maybe (List ℤ)
    unwrap xs = unwrap' (just []) xs where
      unwrap' : Maybe (List ℤ) → List (Maybe ℤ) → Maybe (List ℤ)
      unwrap' (just xs) (just y ∷ ys) = unwrap' (just (Data.List._++_ xs [ y ])) ys  -- makes unwrap O(N^2)!
      unwrap' (just xs) (nothing ∷ _) = nothing
      unwrap' (just xs) []            = just xs
      unwrap' nothing   _             = nothing

    then? : {A : Set} → {B : Set} → Maybe A → (A → Maybe B) → Maybe B
    then? nothing _ = nothing
    then? (just r1) op2 = op2 r1

    digitsToℤ : List ℤ → ℤ
    digitsToℤ xs = digitsToℤ' (reverse xs) where
      digitsToℤ' : List ℤ → ℤ
      digitsToℤ' []       = (+ 0)
      digitsToℤ' (x ∷ xs) = x + ((+ 10) * (digitsToℤ' xs))

ℤ? : Maybe ℤ -> ℤ
ℤ? (just x) = x
ℤ? nothing  = (+ 0)
