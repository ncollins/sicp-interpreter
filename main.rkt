#lang racket/base

(require rackunit)
(require racket/class)
(require racket/list)
(require racket/string)
(require racket/format)

(require "env.rkt")
(require "objects.rkt")

;; The eval function

(define (constant? exp)
  (or (equal? 'null exp)
      (equal? 'true exp)
      (equal? 'false exp)))

(define (self-evaluating? exp)
  (or (constant? exp)
      (number? exp)
      (string? exp)
      (boolean? exp)
      (is-a? exp pair%)))


(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
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
                       (send f function-eval args eval))]
        ; don't know what to do
        [else (error (string-append "Could not evaluate `" (~a exp) "`"))]))

;; eval-tests-basic
(check-eq? 1 (eval 1 '()))

(check-eq? #t (eval #t '()))

(check-eq? (pair-first (pair 1 2)) (pair-first (eval (pair 1 2) empty-env)))

(check-eq? (pair-second (pair 1 2)) (pair-second (eval (pair 1 2) empty-env)))

(check-eq? 13 (eval ((lambda (x) (+ x 10)) 3) '()))

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

;(check-eq? 10 (eval '(if-else ((lambda (x) true) "this is ignored") 10 0) empty-env))


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

