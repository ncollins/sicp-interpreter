#lang racket/base

(provide pair%)
(provide pair)
(provide pair?)
(provide pair-first)
(provide pair-second)
(provide function%)
(provide built-in-function%)


(require rackunit)
(require racket/list)
(require racket/class)
(require "env.rkt")

;; For numbers and strings, the existing Racket datatypes are used, because
;; this means a dedicated parsing function is not needed. Constant values
;; (true, false, null) and variables are stored as Racket symbols
;; for the same reason.

;; So the only objects currently included are:
;; - pairs% (which can be used to build lists)
;; - function% (used for functions defined with the `lambda` form
;; - built-in-function% which is used to create functions written
;;   in Racket for the prelude.

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
    (define/public (function-eval delayed-arguments eval-function)
      ;; create new frame
      ;; TODO - shouldn't be using make-hash here...
      (define frame (make-hash (map cons f-args delayed-arguments)))
      (define env (cons frame f-enclosing-env))
      (eval-function f-body env))))


;; TODO - refactor so this code is not duplicated
;; (currently included to avoid circular dependencies)



;;(define (actual-value exp env)
;;  (force-it (eval exp env)))

(define (actual-value eval-function exp env)
  (force-it eval-function (eval-function exp env)))

;;(define (force-it obj)
;;  (if (thunk? obj)
;;      (actual-value (thunk-exp obj) 
;;                    (thunk-env obj))
;;      obj))


(define (force-it eval-function obj)
  (if (thunk? obj)
      (actual-value eval-function
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

(define built-in-function%
  (class object%
    (init racket-function)
    (super-new)
    (define function racket-function)
    (define/public (self-evaluating?) #f)
    (define/public (function-eval delayed-arguments eval-function)
      ;; TODO - will need a wrapper around apply to handle lazy evaluation
      (let* ([actual-args (map
                           (lambda (thunk) (actual-value eval-function
                                                         (thunk-exp thunk)
                                                         (thunk-env thunk)))
                           delayed-arguments)])
          (apply function actual-args)))))