#lang racket/base

(require rackunit)
(require racket/class)
(require racket/list)
(require racket/string)
(require racket/format)

(require "env.rkt")
(require "objects.rkt")
(require "prelude.rkt")
(require "thunk.rkt")

(provide (all-defined-out))

;; self evaluating

(define (constant? exp)
  (or (equal? 'null exp)
      (equal? 'true exp)
      (equal? 'false exp)))

(define (exp-self-evaluating? exp)
  (or (constant? exp)
      (number? exp)
      (string? exp)
      (is-a? exp pair%)))

(define (eval-self-evaluating exp env)
  exp)

;; if

(define (exp-if? exp)
  (equal? 'if (first exp)))

(define (eval-if exp env)
  (if (equal? (eval-in-env (second exp) env) 'true)
      (eval-in-env (third exp) env)
      (eval-in-env (fourth exp) env)))

;; variable

(define (exp-variable? exp)
  (symbol? exp))

(define (eval-variable exp env)
  (lookup-in-env exp env))

;; define

(define (exp-define? exp)
  (equal? 'define (first exp)))

(define (eval-define exp env)
  (bind-var-in-env! (second exp) (eval-in-env (third exp) env) env))

;; set!

(define (exp-set!? exp)
  (equal? 'set! (first exp)))

(define (eval-set! exp env)
  (set-var-in-env! (second exp) (eval-in-env (third exp) env) env))

;; begin

(define (exp-begin? exp)
  (equal? 'begin (first exp)))

(define (eval-begin exp env)
  (foldl (lambda (e return)
           (eval-in-env e env))
         null
         (rest exp)))

;; let

(define (exp-let? exp)
  (equal? 'let (first exp)))

(define (eval-let exp env)
  (let [(new-env (add-in-new-frame-to-env (first (second exp))
                                          (eval-in-env (second (second exp)) env)
                                          env))]
    (eval-in-env (third exp) new-env)))

;; lambda

(define (exp-lambda? exp)
  (equal? 'lambda (first exp)))

(define (eval-lambda exp env)
  (new function% [args (second exp)] [body (third exp)] [enclosing-env env]))

;; function application

(define (eval-function-application exp env)
  (let ([f (actual-value eval-in-env (first exp) env)]
        [args (map (lambda (e) (delay-it e env)) (rest exp))])
    (send f function-eval args eval-in-env)))

;; evaluate expression in a given environment

(define (eval-in-env exp env)
  (cond
    ;; atoms
    [(exp-self-evaluating? exp)
     (eval-self-evaluating exp env)]
    [(exp-variable? exp)
     (eval-variable exp env)]
    [(not (list? exp))
     (error (string-append "Could not evaluate the atom`" (~a exp) "`"))]
    ;; compound expressions
    [(exp-if? exp)
     (eval-if exp env)]
    [(exp-define? exp)
     (eval-define exp env)]
    [(exp-set!? exp)
     (eval-set! exp env)]
    [(exp-begin? exp)
     (eval-begin exp env)]
    [(exp-let? exp)
     (eval-let exp env)]
    [(exp-lambda? exp)
     (eval-lambda exp env)]
    ;; Any other list is assumed to be function application
    [else
     (eval-function-application exp env)]
    ))

;; eval function wrapping the prelude

(define (eval exp)
  (eval-in-env exp (new-prelude-env)))

(check-eq? 1 (eval 1))

(let [(test-pair (pair 1 2))]
  (check-eq? test-pair (eval test-pair)))

(check-eq? 10 (eval '(+ 5 5)))

;; Test self-evaluating

(check-eq? 1 (eval 1))
(check-eq? "hello world" (eval "hello world"))
(check-eq? 'true (eval 'true))
(check-eq? 'false (eval 'false))
(check-eq? 'null (eval 'null))

(let [(test-pair (pair 1 2))]
  (check-eq? test-pair (eval test-pair)))
(check-eq? 1 (pair-first (eval (pair 1 2))))
(check-eq? 2 (pair-second (eval (pair 1 2))))

;; Test failure of unsupported atoms

(check-exn exn:fail? (lambda () (eval #t)))

;; Test if

(check-eq? 1 (eval '(if true 1 9)))
(check-eq? 9 (eval '(if false 1 9)))

;; Test prelude functions

(check-eq? 14 (eval '(+ 4 10)))
(check-eq? 7 (eval '(first (pair (+ 2 5) 1))))

;; Test lambda and function application

(check-eq? 13 (eval ((lambda (x) (+ x 10)) 3)))

(check-eq? 10 (eval '(if ((lambda (x) true) "this is ignored") 10 0)))

;; Test let

(check-eq? 22 (eval '(let (a 10) (+ a 12))))

(check-eq? 14 (eval '(let (a (+ 1 3)) (+ a 10))))

;; Test begin

(check-eq? 17 (eval '(begin (define a 10) (+ a 7))))

;; lazy tests

(check-eq? 13 (eval '(begin (define a 10)
                            (define f (lambda (x) (+ x 10)))
                            (let (a 3) (f a)))))

;; Composite test #1

(check-eq? 23 (eval '(begin (define a 10)
                            (define change-a (lambda () (set! a 20)))
                            (change-a)
                            (define f (lambda (x) (+ x a)))
                            (let (a 3) (f a)))))

;; Composite test #2
;; (we need to add zero because the fold returns a thunk)

(check-eq? 14 (eval '(begin
                      (define fold
                        (lambda (f acc lst) (if (null? lst)
                                               acc
                                               (let (new-acc (f (first lst) acc))
                                                   (fold f new-acc (second lst))))))
                      (define map
                        (lambda (f lst) (if (null? lst)
                                           null
                                           (pair (f (first lst)) (map f (second  lst))))))
                      (let (numbers (pair 1 (pair 2 (pair 3 null))))
                          (let (squares (map (lambda (x) (* x x)) numbers))
                              (+ 0 (fold + 0 squares)))))))

(check-eq? 1 (eval '(begin
                      (define x 0)
                      (define double (lambda (n) (+ n n)))
                      (define m (double (begin (set! x (+ x 1)) 10)))
                      x)))
