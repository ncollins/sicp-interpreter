#lang racket/base

(provide actual-value)
(provide force-it)
(provide delay-it)
(provide thunk?)
(provide thunk-exp)
(provide thunk-env)

(require racket/list)

(define (actual-value eval-in-env exp env)
  (force-it eval-in-env (eval-in-env exp env)))

(define (force-it eval-in-env obj)
  (if (thunk? obj)
      (actual-value eval-in-env
                    (thunk-exp obj)
                    (thunk-env obj))
      obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? exp)
  (and (list? exp)
       (equal? 'thunk (first exp))))

(define (thunk-exp thunk) (second thunk))
(define (thunk-env thunk) (third thunk))