#lang racket

(require (prefix-in prim: "primitives.rkt"))

(prim:defop (= . xs) (apply equal? xs))
(prim:defop (!= . xs) (not (apply equal? xs)))
(prim:defops* < <= > >=)
