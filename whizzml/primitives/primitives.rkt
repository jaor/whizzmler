#lang racket

(require (prefix-in errors: whizzml/errors)
         (prefix-in types: whizzml/types)
         (for-syntax racket/syntax))

(provide defop defop* defops*)


(define-for-syntax (make-id name)
  (format-id name "whizzml-~a" (syntax-e name)))

(define-syntax-rule (handling body ...)
  (with-handlers ([exn:fail:contract:arity?
                   (lambda (e) (errors:signal-exn 'arity e))]
                  [exn:fail:contract?
                   (lambda (e) (errors:signal-exn 'domain-error e))]
                  [exn? (lambda (e)
                          (errors:signal-exn 'internal-error e))])
    body ...))

(define-syntax (defop stx)
  (syntax-case stx ()
    [(_ (name params ...) body ...)
     (with-syntax ([opname (make-id #'name)])
       #'(begin
           (define (opname params ...) (handling body ...))
           (provide (rename-out [opname name]))))]
    [(_ (name params ... . rest) body ...)
     (with-syntax ([opname (make-id #'name)])
       #'(begin
           (define (opname params ... . rest) (handling body ...))
           (provide (rename-out [opname name]))))]))

(define-syntax-rule (defop* name) (defop (name . rest) (apply name rest)))
(define-syntax-rule (defops* name ...) (begin (defop* name) ...))
