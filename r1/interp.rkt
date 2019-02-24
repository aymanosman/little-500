#lang racket

(provide interp-R1)

(require "utils.rkt")

(define (interp-R1 env)
  (define (interp-exp env)
    (lambda (e)
      (match e
        [(? fixnum?) e]
        [`(read)
         (define r (read))
         (if (fixnum? r)
             r
             (error `interp-R1 "input not an integer" r))]
        [`(- ,e)
         (define v ((interp-exp env) e))
         (- 0 v)]
        [`(- ,es ...)
         (apply -
                (for/list ([e es])
                  ((interp-exp env) e)))]
        [`(+ ,es ...)
         (apply +
                (for/list ([e es])
                  ((interp-exp env) e)))]
        [(? symbol?)
         (let ([res (a-get e env)])
           (if res
               res
               (error `interp-R1 "Symbol lookup failed:" e)))]
        [`(let ([,x ,e]) ,body)
         (define new-env
           (cons (cons x ((interp-exp env) e))
                 env))
         ((interp-exp new-env) body)])))
  (lambda (p)
    (match p
      [`(program ,info ,e)
       ((interp-exp '()) e)])))

;; TODO: implement c0 interpreter
(define (interp-C0 env)
  (define (interp-exp env)
    (lambda (e)
      (match e
        [else e])))
  (lambda (p)
    (match p
      [`(program ,info ,e)
       ((interp-exp '()) e)])))

(module+ test
  (require rackunit
           "test/test-programs.rkt")

  (define answer-to-the-great-question
    '(program ()
              (+ 45 (+ (+ (let ([y (+ 0 (- 15))])
                            y) 2)
                       10))))

  (test-case "Example-based test"
             (check-eq? ((interp-R1 '()) answer-to-the-great-question)
                        42)))
