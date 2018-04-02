#lang racket

(require (prefix-in prim: "primitives.rkt"))

(define-syntax-rule (binops (fun name) ...)
  (begin (prim:defop (name x y) (fun x y)) ...))

(prim:defops* + - * / abs sqrt)

(binops [modulo mod]
        [expt pow]
        [remainder rem]
        [quotient div])

(define (app-to-numbers fn args)
  (cond [(and (= 1 (length args)) (list? (car args))) (apply fn args)]
        [(= 1 (length args)) (car args)]
        [else (apply fn args)]))

(prim:defop (max . args) (app-to-numbers max args))
(prim:defop (min . args) (app-to-numbers max args))

(prim:defops* exp log sinh cosh tanh)
(prim:defop (log10 x) (log x 10))
(prim:defop (log2 x) (log x 2))

(prim:defops* sin cos tan atan acos asin)
(prim:defop (to-radians x) (degrees->radians x))
(prim:defop (to-degrees x) (radians->degrees x))

(prim:defop (ceil x) (exact-ceiling x))
(prim:defop (floor x) (exact-floor x))
(prim:defop (round x) (exact-round x))

(prim:defops* zero? positive? negative? odd? even?)

(require (prefix-in stats: math/statistics))
(prim:defop (mean ns) (stats:mean ns))
(prim:defop (variance ns) (stats:variance ns))
(prim:defop (stdev ns) (stats:stddev ns))

(define (npdf x mn std)
  (let* ([var (* std std)]
         [denom (* std (sqrt (* 2 pi)))]
         [num (exp (/ (- (expt (- x mn) 2)) (* 2 var)))])
    (/ num denom)))

(define integral-slices 512.0)
(define (trapezoid-area x1 x2 y1 y2)
  (let* ([run (abs (- x1 x2))]
         [rect-rise (min (abs y1) (abs y2))]
         [tri-rise (- (max (abs y1) (abs y2)) rect-rise)]
         [rect-area (* run rect-rise)]
         [tri-area (/ (* run tri-rise) 2)])
    (+ rect-area tri-area)))

(define (trap-integral f xmin xmax)
  (let* ([step (/ (- xmax xmin) integral-slices)]
         [xs (range xmin (+ xmax (/ step 2)) step)]
         [ys (map f xs)])
    (foldl + (map trapezoid-area xs (cdr xs) ys (cdr ys)))))

(define gauss-integral-limit -16)

(prim:defop (bigml--pdf x mn std) (npdf x mn std))
(prim:defop (bigml--cdf x mn std)
  (if (zero? std)
      (if (> x mn) 1.0 0.0)
      (let ([stds (/ (- x mn) std)]
            [norm-fn (lambda (x) (npdf x 0 1))])
        (trap-integral norm-fn gauss-integral-limit stds))))
