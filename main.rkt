#lang racket/base

(require rackunit)
(require racket/class)
(require racket/list)
(require racket/string)
(require racket/format)

(require "env.rkt")

;; We want to support:
;; - numbers
;; - bools
;; - strings
;; - lists
;; - lambdas

(define expression-interface<%>
  (interface () self-evaluating?))

(define pair%
  (class object%
    (init first second)
    (define fst first)
    (define snd second)
    (define/public (get-first) fst)
    (define/public (get-second) snd)
    ;(define v value)
    (super-new)
    ;(define/public (value) value)
    (define/public (self-evaluating?)
      #t)))

(define (pair a b)
  (new pair% [first a] [second b]))

(define (pair? x)
  (is-a? x pair%))

(define (pair-first x)
  (send x get-first))

(define (pair-second x)
  (send x get-second))

;; Variables

(define variable%
  (class object%
    (init symbol)
    (define var-symbol symbol)
    (define/public (self-evaluating?) #f)
    (define/public (eval-in-env env)
      (lookup-in-env var-symbol))))

;; variables-tests

(let ([env (bind-var-in-env! 'x 10 empty-env)])
  (check-eq? (lookup-in-env 'x env) 10))

(let* ([env (bind-var-in-env! 'x 10 empty-env)]
       [env (bind-var-in-env! 'x 12 env)])
  (check-eq? (lookup-in-env 'x env) 12))

;; Lambdas/functions consist of:
;; - parameters
;; - a body
;; - the environment the function was defined it
;; and they are evaluated by creating a frame binding the arguments
;; to the parameter variables and adding that frame as a child to
;; the environment.

(define function%
  (class object%
    (init args body enclosing-env)
    (super-new)
    (define f-args args)
    (define f-body body)
    (define f-enclosing-env enclosing-env)
    (define/public (self-evaluating?) #f)
    (define/public (function-eval params)
      ;; create new frame
      ;; TODO - shouldn't be using make-hash here...
      (define frame (make-hash (map cons f-args params)))
      (define env (cons frame f-enclosing-env))
      (eval f-body env))))

(define built-in-function%
  (class object%
    (init racket-function)
    (super-new)
    (define function racket-function)
    (define/public (self-evaluating?) #f)
    (define/public (function-eval params)
      (apply function params))))]

;; eval-tests-basic
(check-eq? 1 (eval 1 '()))

(check-eq? #t (eval #t '()))

(check-eq? (pair-first (pair 1 2)) (pair-first (eval (pair 1 2) empty-env)))

(check-eq? (pair-second (pair 1 2)) (pair-second (eval (pair 1 2) empty-env)))

(check-eq? 13 (eval ((lambda (x) (+ x 10)) 3) '()))

;; The eval function

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (boolean? exp)
      ;(send (first exp) self-evaluating?)))
      (is-a? exp pair%)))

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        ; constants
        [(equal? 'null exp) 'null]
        [(equal? 'true exp) 'true]
        [(equal? 'false exp) 'false]
        ; variable
        [(symbol? exp) (lookup-in-env exp env)]
        ; if-else
        [(equal? 'if-else (first exp))
         (if (equal? (eval (second exp) env) 'true)
             (eval (third exp) env)
             (eval (fourth exp) env))]
        ; define
        [(equal? 'define (first exp))
         (bind-var-in-env! (second exp) (eval (third exp) env) env)]
        ; set!
        [(equal? 'set! (first exp))
         (set-var-in-env! (second exp) (third exp) env)]
        ; begin
        [(equal? 'begin (first exp))
         (foldl (lambda (e return)
                  (eval e env))
                null
                (rest exp))]
        ; with (i.e. let)
        [(equal? 'with (first exp))
         (let [(new-env (add-in-new-frame-to-env (first (second exp)) (second (second exp)) env))]
           (eval (third exp) new-env))]
        ; lambda expression
        [(equal? 'lambda (first exp))
         (new function% [args (second exp)] [body (third exp)] [enclosing-env env])]
        ; for anything else do we just eval the parts and then apply?
        [(list? exp) (let ([f (eval (first exp) env)]
                           [args (map (lambda (e) (eval e env)) (rest exp))])
                       (send f function-eval args))]
        ; don't know what to do
        [else (error (string-append "Could not evaluate `" (~a exp) "`"))]))

(let* ([env empty-env]
       [plus (new built-in-function% [racket-function +])]
       [env (bind-var-in-env! 'plus plus env)])
  (check-eq? 14 (eval (list 'plus 4 10) env)))

(let* ([env empty-env]
       [plus (new built-in-function% [racket-function +])]
       [env (bind-var-in-env! 'plus plus env)]
       [env (bind-var-in-env! 'x 99 env)])
  (check-eq? 103 (eval (list 'plus 4 'x) env)))

(check-eq? 10 (eval '(if-else true 10 0) empty-env))
(check-eq? 0 (eval '(if-else false 10 0) empty-env))

(check-eq? 10 (eval '(if-else ((lambda (x) true) "this is ignored") 10 0) empty-env))


(define base-env
  (let* ([env empty-env]
         [env (bind-var-in-env! 'null? (new built-in-function% [racket-function (lambda (x) (equal? x 'null))]) env)]
         [env (bind-var-in-env! 'pair (new built-in-function% [racket-function pair]) env)]
         [env (bind-var-in-env! 'first (new built-in-function% [racket-function pair-first]) env)]
         [env (bind-var-in-env! 'second (new built-in-function% [racket-function pair-second]) env)]
         [env (bind-var-in-env! '+ (new built-in-function% [racket-function +]) env)])
    env))

(check-eq? 34 (eval '(if-else true (+ 30 4) 0) base-env))

(let [(env (bind-var-in-env! 'x 967 empty-env))]
  (check-eq? 967 (eval 'x env)))

(check-eq? 22 (eval '(with (a 10) (+ a 12)) base-env))

(check-eq? 17 (eval '(begin (define a 10) (+ a 7)) base-env))

(check-eq? 23 (eval '(begin (define a 10)
                            (define change-a (lambda () (set! a 20)))
                            (change-a)
                            (define f (lambda (x) (+ x a)))
                            (with (a 3) (f a)))
                    base-env))

(check-eq? 7 (eval '(first (pair (+ 2 5) 1)) base-env))

