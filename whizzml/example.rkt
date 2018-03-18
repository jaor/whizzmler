#lang whizzml

(define a 3)
(define b -1)
(define (foo x y) (+ x y))

(define mm {"a" "b"})

(define ll [{"a" 2 "b" 8} true 23.2])

(define mu (lambda (x . y)
             (cond (> 23 x 2) "foo"
                   (> x 23) "bar"
                   "baz")))

(define (=? x y)
  (or (= x y) (raise "test failure")))

(let (lst [0 43 3])
  (=? (for (x lst) (mu x)) [(mu 0) (mu 43) (mu 3)]))

(=? 0 (loop (x 100) (if (> x 0) (recur (- x 1)) x)))

(=? 20 (iterate (a -1 b [1 2 3] c [4 5 6]) (+ a b c)))

(=? "foo"
    (iterate (a -1 b [1 2 3] c [4 5 6]) (if (> a 5) (break "foo") (+ a b c))))

(=? [1 2] (handle (lambda (e) e) (+ 1 (raise [1 2]))))

(=? "oh" (try (+ 1 (ll 2)) (raise "oh") (catch xxx xxx)))

(=? true true)

(define [left right] [1 2 3])
(=? left 1)
(=? right 2)

(define defined-list ["a" 2])

(=? (defined-list 0) (let ([a b] defined-list) a))
