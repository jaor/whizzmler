#lang racket

(provide #%module-begin)
(provide #%datum)
(provide #%top #%top-interaction)

(require (for-syntax (prefix-in parser: "parser.rkt")))

(define-syntax-rule (whizzml-program FORM ...)
  (begin FORM ... ))
(provide whizzml-program)

(define-syntax-rule (number N) N)
(define-syntax-rule (string S) S)
(define-syntax-rule (bool B) B)
(provide number string bool ident)

(require (for-syntax racket/syntax))

(define-for-syntax (make-id name)
  (format-id name "~a" (syntax-e name)))

(define-syntax (ident stx)
  (syntax-case stx ()
    [(_ name) (make-id #'name)]))

(define-syntax (map-lit stx)
  (syntax-case stx (kvpair)
    [(_ (kvpair k v) ...) #'(make-immutable-hash (list (cons k v) ...))]))
(provide map-lit)

(define-syntax (list-lit stx)
  (syntax-case stx ()
    [(_ elem ...) #'(list elem ...)]))
(provide list-lit)

(define defined-list (make-parameter void))
(define (pop-defined)
  (when (empty? (defined-list)) (errors:signal 'list-too-small))
  (begin0 (car (defined-list)) (defined-list (cdr (defined-list)))))

(define-syntax (definition stx)
  (syntax-case stx (ident ident-list funsign formals defined-list)
    [(_ (ident name) v) #`(define #,(make-id #'name) v)]
    [(_ (ident-list) ignored) #'(defined-list void)]
    [(_ (ident-list name names ...) defined-list)
     #'(begin (definition (ident name) (pop-defined))
              (definition (ident-list names ...) defined-list))]
    [(_ (ident-list name names ...) value)
     #'(begin (defined-list value)
              (definition (ident-list name names ...) defined-list))]
    [(_ (funsign name args ...) body)
     #`(define #,(make-id #'name) (w-lambda (formals args ...) body))]))
(provide definition)

(require (prefix-in errors: "errors.rkt"))

(define-syntax-rule (w-raise v) (raise (errors:whizzml-error v)))

(define-syntax handle
  (syntax-rules (body)
    [(_ HANDLER (body FORMS ...))
     (with-handlers ([errors:whizzml-error?
                      (lambda (e) (HANDLER (errors:whizzml-error-value e)))])
       FORMS ...)]))

(define-syntax (catch stx)
  (syntax-case stx (body)
    [(_ (ident e) forms ...) #`(lambda (#,(make-id #'e)) forms ...)]))

(define-syntax-rule (try forms ... catcher)
  (handle catcher forms ...))

(provide handle try catch (rename-out [w-raise raise]))

(define-syntax w-if
  (syntax-rules ()
    [(_ COND THEN) (if COND THEN #f)]
    [(_ COND THEN ELSE) (if COND THEN ELSE)]))
(provide (rename-out [w-if if]))

(define-syntax-rule (w-when TEST BODY ...)
  (if TEST (begin BODY ...) #f))
(provide (rename-out [w-when when]))

(define-syntax w-cond
  (syntax-rules (branch def)
    [(_ (branch TEST BRANCH)) (if TEST BRANCH #f)]
    [(_ (branch TEST BRANCH) (def DEF)) (if TEST BRANCH DEF)]
    [(_ (branch TEST BRANCH) MORE ...) (if TEST BRANCH (w-cond MORE ...))]))
(provide (rename-out [w-cond cond]))

(define-syntax (w-for stx)
  (syntax-case stx ()
    ((_ (ident name) lst (body forms ...))
     #`(map (lambda (#,(make-id #'name)) forms ...) lst))))
(provide (rename-out [w-for for]))

(require (for-syntax (prefix-in rl: racket/list)))

(define w-recur (make-parameter (void)))
(define (recur . args) (apply (w-recur) args))

(define-syntax (loop stx)
  (syntax-case stx (bindings binding body ident)
    [(_ (bindings (binding (ident name) value) ...) (body forms ...))
     (with-syntax ([[id ...] (map make-id (syntax-e #'(name ...)))]
                   [[v ...] (syntax-e #'(value ...))])
       #'(parameterize ((w-recur (lambda (id ...) forms ...)))
           (recur v ...)))]))
(provide loop recur)

(define-struct break-exn (value))
(define (break v) (raise (make-break-exn v)))

(define-syntax (iterate stx)
  (syntax-case stx (bindings binding body ident)
    [(_ (bindings (binding (ident acc) init) (binding (ident x) v) ...)
        (body forms ...))
     (with-syntax ([[x ...] (map make-id (syntax-e #'(x ...)))]
                   [[v ...] (syntax-e #'(v ...))]
                   [acc (make-id #'acc)])
       #`(with-handlers ([break-exn? break-exn-value])
           (for/fold ([acc init]) ([x v] ...) forms ...)))]))
(provide iterate break)

(define-syntax (w-let stx)
  (syntax-case stx (ident ident-list bindings binding body)
    [(_ (bindings (binding (ident-list names ...) value) more ...) b)
     (with-syntax ([[bs ...]
                    (map (lambda (name pos)
                           (list (make-id name) #`(list-ref v #,pos)))
                         (syntax-e #'(names ...))
                         (rl:range (length (syntax-e #'(names ...)))))])
       #'(let* ([v value] bs ...)
           (w-let (bindings  more ...) b)))]
    [(_ (bindings (binding (ident name) value) more ...) (body forms ...))
     #`(let ([#,(make-id #'name) value])
         (w-let (bindings more ...) (body forms ...)))]
    [(_ (bindings) (body forms ...)) #'(let () forms ...)]))
(provide (rename-out [w-let let]))

(define-syntax (w-lambda stx)
  (syntax-case stx (name formals opt)
    [(_ (formals args ... (opt opt-arg)) (body forms ...))
     (with-syntax ([[id ...] (map make-id (syntax-e #'(args ...)))]
                   [opt (make-id #'opt-arg)])
       #'(lambda (id ... . opt) forms ...))]
    [(_ (formals args ...) (body forms ...))
     (with-syntax ([ids (map make-id (syntax->list #'(args ...)))])
       #'(lambda ids forms ...))]
    [(_ (name fname) (formals args ...) (body forms ...))
     (with-syntax ([id (make-id #'fname)])
       #'(letrec ((id (w-lambda (formals args ...) (body forms ...)))) id))]))
(provide (rename-out [w-lambda lambda]))

(require (prefix-in containers: "containers.rkt"))

(define (w-apply f . args)
  (cond [(procedure? f) (apply f args)]
        [(not (or (hash? f) (list? f)))
         (errors:signal 'domain-error "expected procedure, list or map")]
        [(not (< 0 (length args) 3)) (errors:signal-arity 1 2)]
        [else (apply containers:get-in f args)]))

(define-syntax (app stx)
  (syntax-case stx (ident)
    [(_ f args ...) #'(w-apply f args ...)]))
(provide app (rename-out [app #%app]))

(provide + - * / abs acos asin atan (rename-out [modulo mod] [equal? =])
         < <= > >= and or not)
