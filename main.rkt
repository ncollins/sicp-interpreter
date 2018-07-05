#lang racket/base

(require rackunit)
(require racket/class)
(require racket/list)
(require racket/string)
(require racket/format)

(require "env.rkt")
(require "objects.rkt")

;; self evaluating

(define (constant? exp)
  (or (equal? 'null exp)
      (equal? 'true exp)
      (equal? 'false exp)))

(define (exp-self-evaluating? exp)
  (or (constant? exp)
      (number? exp)
      (string? exp)
      ;(boolean? exp)
      (is-a? exp pair%)))

(define (eval-self-evaluating exp env)
  exp)



;; if

(define (exp-if? exp)
  (equal? 'if-else (first exp)))

(define (eval-if exp env)
  (if (equal? (eval (second exp) env) 'true)
      (eval (third exp) env)
      (eval (fourth exp) env)))

;; variable

(define (exp-variable? exp)
  (symbol? exp))

(define (eval-variable exp env)
  (lookup-in-env exp env))

;; define

(define (exp-define? exp)
  (equal? 'define (first exp)))

(define (eval-define exp env)
  (bind-var-in-env! (second exp) (eval (third exp) env) env))

;; set!

(define (exp-set!? exp)
  (equal? 'set! (first exp)))

(define (eval-set! exp env)
  (set-var-in-env! (second exp) (third exp) env))

;; begin

(define (exp-begin? exp)
  (equal? 'begin (first exp)))

(define (eval-begin exp env)
  (foldl (lambda (e return)
           (eval e env))
         null
         (rest exp)))

;; with/let

(define (exp-with? exp)
  (equal? 'with (first exp)))

(define (eval-with exp env)
  (let [(new-env (add-in-new-frame-to-env (first (second exp)) (second (second exp)) env))]
    (eval (third exp) new-env)))

;; lambda

(define (exp-lambda? exp)
  (equal? 'lambda (first exp)))

(define (eval-lambda exp env)
  (new function% [args (second exp)] [body (third exp)] [enclosing-env env]))

;; function application

(define (eval-function-application exp env)
  (let ([f (eval (first exp) env)]
        [args (map (lambda (e) (eval e env)) (rest exp))])
    (send f function-eval args eval)))
  

;; eval

(define (eval exp env)
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
    [(exp-with? exp)
     (eval-with exp env)]
    [(exp-lambda? exp)
     (eval-lambda exp env)]
    ;; Any other list is assumed to be function application
    [else
     (eval-function-application exp env)]
    ))

;; Test self-evaluating

(check-eq? 1 (eval 1 empty-env))
(check-eq? "hello world" (eval "hello world" empty-env))
(check-eq? 'true (eval 'true empty-env))
(check-eq? 'false (eval 'false empty-env))
(check-eq? 'null (eval 'null empty-env))

(let [(test-pair (pair 1 2))]
  (check-eq? test-pair (eval test-pair empty-env)))
(check-eq? 1 (pair-first (eval (pair 1 2) empty-env)))
(check-eq? 2 (pair-second (eval (pair 1 2) empty-env)))

;; Test failure of unsupported atoms

(check-exn exn:fail? (lambda () (eval #t '())))

;; Test if

(check-eq? 1 (eval '(if-else true 1 9) empty-env))
(check-eq? 9 (eval '(if-else false 1 9) empty-env))

;; Eval function tests



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

