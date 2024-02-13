{-# OPTIONS --guardedness #-}

module Main where

open import IO

main : Main
main = run (putStrLn "hello, world")
