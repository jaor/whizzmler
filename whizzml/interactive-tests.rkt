#lang racket

(require (prefix-in tok: "tokenizer.rkt")
         (prefix-in parser: "parser.rkt")
         (prefix-in main: "main.rkt")
         (prefix-in ru: br/reader-utils)
         (prefix-in su: brag/support))

(define (apply-tokenizer str)
  (su:apply-tokenizer-maker tok:make-tokenizer str))

(define (parse-str str)
  (parser:parse-to-datum (apply-tokenizer str)))

(define (read-str str)
  (ru:test-reader main:read-syntax str))
