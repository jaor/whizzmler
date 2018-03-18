#lang racket/base

(require (prefix-in tokenizer: "tokenizer.rkt")
         (prefix-in parser: "parser.rkt")
         (prefix-in sc: syntax/strip-context)
         (prefix-in brr: br/reader-utils))

(module+ reader
  (provide read-syntax read get-info))

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/default-lexer 'default-lexer)]
      [else default])))

(define (read-syntax path port)
  (sc:strip-context
   #`(module whizzml-module whizzml/expander
       #,(parser:parse path (tokenizer:make-tokenizer port)))))

(define (read port)
  (syntax->datum (read-syntax #f port)))

(provide read read-syntax)
