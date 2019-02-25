#lang racket

(provide interp-R1
         interp-C0)

(require "utils.rkt")

(define (eval-exp env)
  (lambda (e)
    (match e
      [(? fixnum?) e]
      [`(read)
       (define r (read))
       (if (fixnum? r)
           r
           (error `interp-R1 "input not an integer" r))]
      [`(- ,e)
       (define v ((eval-exp env) e))
       (- 0 v)]
      [`(- ,es ...)
       (apply -
              (for/list ([e es])
                ((eval-exp env) e)))]
      [`(+ ,es ...)
       (apply +
              (for/list ([e es])
                ((eval-exp env) e)))]
      [(? symbol?)
       (let ([res (a-get e env)])
         (if res
             res
             (error `interp-R1 "Symbol lookup failed:" e)))]
      [`(let ([,x ,e]) ,body)
       (define new-env
         (cons (cons x ((eval-exp env) e))
               env))
       ((eval-exp new-env) body)])))

(define (interp-R1 env)
  (lambda (p)
    (match p
      [`(program ,info ,e)
       ((eval-exp '()) e)])))

(define (interp-C0 env)
  (define (update-env env e)
    (match e
      [`(assign ,x ,b)
       (cons `(,x . ,((eval-exp env) b))
             env)]))
  (define (interp-tail env)
    (lambda (e)
      (match e
        [`(seq ,assignment ,tail)
         (define new-env (update-env env assignment))
         ((interp-tail new-env) tail)]
        [`(return ,k)
         ((eval-exp env) k)])))
  (lambda (p)
    (match p
      [`(program ,info ,e)
       ((interp-tail '()) e)])))

(module+ test
  (require rackunit)

  (define R1-example
    '(program ()
              (+ 45 (+ (+ (let ([y (+ 0 (- 15))])
                            y) 2)
                       10))))

  (define C0-example
    '(program ()
              (seq (assign x.1 (+ 2 2))
                   (seq (assign x.2 38)
                        (seq (assign y (+ x.1 x.2))
                             (return y))))))

  (test-case "Example-based test of interp-R1"
             (check-eq? ((interp-R1 '()) R1-example)
                        42))

  (test-case "Example-based test of interp-C0"
             (check-eq? ((interp-C0 '()) C0-example)
                        42)))
