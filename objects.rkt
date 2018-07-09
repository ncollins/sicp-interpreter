#lang racket/base

(provide pair%)
(provide pair)
(provide pair?)
(provide pair-first)
(provide pair-second)
(provide function%)
(provide built-in-function%)


(require rackunit)
(require racket/class)
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

(let ([env (empty-env)])
  (begin
    (bind-var-in-env! 'x 10 env)
    (check-eq? (lookup-in-env 'x env) 10)))

(let* ([env (empty-env)])
  (begin
    (bind-var-in-env! 'x 10 env)
    (bind-var-in-env! 'x 12 env)
    (check-eq? (lookup-in-env 'x env) 12)))

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
    (define/public (function-eval params eval-function)
      ;; create new frame
      ;; TODO - shouldn't be using make-hash here...
      (define frame (make-hash (map cons f-args params)))
      (define env (cons frame f-enclosing-env))
      (eval-function f-body env))))

(define built-in-function%
  (class object%
    (init racket-function)
    (super-new)
    (define function racket-function)
    (define/public (self-evaluating?) #f)
    (define/public (function-eval params eval-function)
      (apply function params))))