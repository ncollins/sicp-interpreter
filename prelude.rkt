#lang racket/base

(provide new-prelude-env)

(require racket/class)

(require "env.rkt")
(require "objects.rkt")

(define (new-prelude-env)
  (let [(env (empty-env))]
    (begin
      (bind-var-in-env! 'null?
                        (new built-in-function% [racket-function (lambda (x) (if (equal? x 'null) 'true 'false))])
                        env)
      (bind-var-in-env! 'pair
                        (new built-in-function% [racket-function pair])
                        env)
      (bind-var-in-env! 'first
                        (new built-in-function% [racket-function pair-first])
                        env)
      (bind-var-in-env! 'second
                        (new built-in-function% [racket-function pair-second])
                        env)
      (bind-var-in-env! '+
                        (new built-in-function% [racket-function +])
                        env)
      (bind-var-in-env! '-
                        (new built-in-function% [racket-function -])
                        env)
      (bind-var-in-env! '*
                        (new built-in-function% [racket-function *])
                        env)
      (bind-var-in-env! '/
                        (new built-in-function% [racket-function /])
                        env)
      
      env)))