#lang racket

(provide explicate-control)

(define (ec-tail e)
  (match e
    [(? symbol?) `(return ,e)]
    [`(return ,exp) e]
    [`(seq ,rest ...) e]
    [`(let ([,x ,b]) ,k)
     (define new-k (ec-tail k))
     (ec-assign b x new-k)]
    [`(- ,es ...) `(return ,e)]
    [`(+ ,es ...) `(return ,e)]
    [else
     (exn `ec-tail "no matching clause" e)]))

(define (ec-assign b x k)
  (match b
    [(? symbol?)
     (define new-k (ec-tail k))
     `(seq (assign ,x ,b) ,new-k)]
    [(? fixnum?)
     `(seq (assign ,x ,b) ,k)]
    [`(let ([,x1 ,b1]) ,k1)
     (define new-k
       (ec-assign k1 x (ec-tail k)))
     (ec-assign b1 x1 new-k)]
    [`(,op ,es ...)
     `(seq (assign ,x ,b) ,(ec-tail k))]
    [else
     (exn `ec-assign "no matching clause" b)]))

(define (explicate-control e)
  (match e
    [`(program ,info ,exp)
     `(program ,info ,(ec-tail exp))]))

(module+ test
  (require rackunit
           "interp.rkt"
           "remove-complex-opera.rkt"
           "test/test-programs.rkt"
           "uniquify.rkt")

  (define pretreat-R1
    (compose remove-complex-opera*
             (uniquify '())))

  (for-each
   (lambda (program)
     (define pretreated-program (pretreat-R1 program))
     (test-eq? "Transformation to C0 preserves meaning"
               ((interp-C0 '())
                (explicate-control pretreated-program))
               ((interp-R1 '()) pretreated-program)))
   R1-examples))
