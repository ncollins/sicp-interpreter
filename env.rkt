#lang racket/base

(provide empty-env)
(provide var-in-env?)
(provide lookup-in-env)
(provide bind-var-in-env!)
(provide set-var-in-env!)
(provide add-in-new-frame-to-env)

(require racket/list)

(define empty-env null)

(define (var-in-env? var env)
	(findf (lambda (e) (hash-has-key? e var)) env))

(define (lookup-in-env var env)
	(if (null? env)
		(error (string-append "No such variable in environment: " (symbol->string var)))
		(hash-ref (first env) var (lambda () (lookup-in-env var (rest env))))))

(define (bind-var-in-env! var val env) ; adds var in first frame (or creates first frame)
	(if (null? env)
		(list (make-hash (list (cons var val))))
		(begin (hash-set! (first env) var val)
					 env)))

(define (set-var-in-env! var val env) ; makes change in first frame where var is found
	(if (null? env)
		(error (string-append "Cannot set variable before definition: " (symbol->string var)))
		(if (hash-has-key? (first env) var)
			(hash-set! (first env) var val)
			(set-var-in-env! var val (rest env)))))


(define (add-in-new-frame-to-env var val env)
	(cons (make-hash (list (cons var val))) env))
