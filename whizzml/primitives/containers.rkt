#lang racket

(provide get-in)

(require (prefix-in errors: whizzml/errors))

(define (not-found . def)
  (lambda () (if (empty? def) (errors:signal 'key-not-found) (car def))))

(define (get1 fl k nf)
  (cond [(list? fl) (cond [(not (integer? k))
                           (errors:signal 'domain-error
                                          "expected number, given " k)]
                          [(not (< -1 k (length fl))) (nf)]
                          [else (list-ref fl k)])]
        [(hash? fl) (if (string? k)
                        (hash-ref fl k nf)
                        (errors:signal 'domain-error
                                       "expected string, given " k))]
        [else (errors:signal 'domain-error "expected list or map, given " fl)]))

(define (get-in fl ks . def)
  (let ([nf (apply not-found def)]
        [ks (if (or (string? ks) (integer? ks)) (list ks) ks)])
    (foldl (lambda (k fl) (get1 fl k nf)) fl ks)))
