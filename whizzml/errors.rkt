#lang racket

(provide (struct-out whizzml-error)
         make-system-error
         signal
         signal-arity)

(struct whizzml-error (value) #:transparent)
(struct whizzml-system-error whizzml-error () #:transparent)

(define error-infos
  (make-hash '((internal-error . (-1 . "Unexpected exception"))
               (divide-by-zero . (-10 . "Divide by zero"))
               (key-not-found . (-15 . "Key not found"))
               (empty-list . (-20 . "Invalid operation on empty list"))
               (list-too-small . (-25 . "List is too short"))
               (domain-error . (-30 . "Invalid argument value"))
               (wrong-arity . (-40 . "Incorrect number of arguments"))
               (resource-error . (-50 "Error handling resource"))
               (primitive-exception . (-100 "Error executing primitive")))))

(define (error-info name)
  (hash-ref error-infos name #(hash-ref error-infos 'internal-error)))

(define (make-system-error name . msgs)
  (let* ([info (error-info name)]
         [msg (or (and (empty? msgs) "")
                  (string-join (map ~a msgs)
                               #:before-first " ("
                               #:after-last ")"))]
         [msg (string-append (cdr info) msg)])
    (whizzml-system-error (make-immutable-hash `(("code" . ,(car info))
                                                 ("message" . ,msg))))))

(define (signal name . msgs)
  (raise (apply make-system-error name msgs)))

(define (signal-arity from to . msgs)
  (apply signal
         'wrong-arity
         (cond [(= from to) (~a from)]
               [(number? to) (~a from " to " to)]
               [else (~a from " or more")])
         (if (empty? msgs) "" "-")
         msgs))
