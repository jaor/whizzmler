#lang racket

(require (prefix-in lex: parser-tools/lex)
         (prefix-in bs: brag/support))

(define (parse-num lexeme)
  (let* ([s (string-split lexeme "r")]
         [b (if (= 1 (length s)) 10 (string->number (car s)))])
    (string->number (last s) b)))

(define id-syms "!$%&*+-./<=>?@^-~")

(define (make-tokenizer port)
  (port-count-lines! port)
  (define (next-token)
    (define whizzml-lexer
      (bs:lexer-srcloc
       [(eof) eof]
       [(bs:from/to ";" "\\n") (bs:token 'COMMENT bs:lexeme #:skip? #t)]
       ["," (next-token)]
       [(bs:char-set "{}[]()") bs:lexeme]
       [(bs::or "if" "cond" "when" "and" "or") bs:lexeme]
       [(bs::or "prog" "define" "let" "lambda" ".") bs:lexeme]
       [(bs::or "loop" "recur" "iterate" "break" "for") bs:lexeme]
       [(bs::or "raise" "handle" "try" "catch") bs:lexeme]
       ["true" (bs:token 'TRUE #t)]
       ["false" (bs:token 'FALSE #f)]
       [(bs::: (bs::? (bs:char-set "+-"))
               (bs::* bs:numeric)
               "."
               (bs::+ bs:numeric)
               (bs::? (bs::: (bs:char-set "eE") (bs::+ bs:numeric))))
        (bs:token 'REAL (string->number bs:lexeme))]
       [(bs::: (bs::? (bs:char-set "+-"))
               (bs::* bs:numeric)
               (bs::: (bs:char-set "eE")
                      (bs::: (bs::? (bs:char-set "+-")) (bs::+ bs:numeric))))
        (bs:token 'REAL (string->number bs:lexeme))]
       [(bs::: "0" (bs::+ (bs::/ "0" "7")))
        (bs:token 'INTEGER (string->number bs:lexeme 8))]
       [(bs::: "0x" (bs::+ (bs::/ "0" "9" "a" "f" "A" "F")))
        (bs:token 'INTEGER (string->number (substring bs:lexeme 2) 16))]
       [(bs::: (bs::+ bs:numeric) "/" (bs::+ bs:numeric))
        (bs:token 'RATIONAL (string->number bs:lexeme))]
       [(bs::: (bs::+ bs:numeric) "r" (bs::? (bs:char-set "+-"))
               (bs::+ (bs::/ "0" "9" "a" "f" "A" "F")))
        (bs:token 'INTEGER (parse-num bs:lexeme))]
       [(bs::: "\"" (bs::* (bs::or (bs::: "\\" bs:any-char)
                                   (bs::~ (bs:char-set "\\\""))))
               "\"")
        (bs:token 'STRING (with-input-from-string bs:lexeme read))]
       [(bs::: (bs::? (bs:char-set "+-")) (bs::+ bs:numeric))
        (bs:token 'INTEGER (string->number bs:lexeme))]
       [(bs::: (bs::or bs:alphabetic (bs:char-set "!$%&*+-./<=>?@^-~"))
               (bs::* (bs::or bs:alphabetic
                              bs:numeric
                              (bs:char-set "!$%&*+-./<=>?@^-~"))))
        (bs:token 'IDENT bs:lexeme)]
       [bs:whitespace (next-token)]
       [bs:any-char (bs:token 'CHAR bs:lexeme)]))
    (whizzml-lexer port))
  next-token)
(provide make-tokenizer)
