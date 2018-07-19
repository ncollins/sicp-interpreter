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
  (cond [(thunk? obj) (let ([computed
                              (actual-value eval-in-env
                                (thunk-exp obj)
                                (thunk-env obj))])
                          (begin 
                            (set-mcar! obj 'computed-thunk)
                            (set-mcdr! obj computed)
                            computed))]
        [(computed-thunk? obj)
         (computed-thunk-value obj)]
      [else obj]))

(define (delay-it exp env)
  (mcons 'thunk (mcons exp env)))

(define (thunk? exp)
  (and (mpair? exp)
       (equal? 'thunk (mcar exp))))

(define (computed-thunk? exp)
  (and (mpair? exp)
       (equal? 'computed-thunk (mcar exp))))

(define (computed-thunk-value obj)
  (mcdr obj))

(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcdr (mcdr thunk)))
