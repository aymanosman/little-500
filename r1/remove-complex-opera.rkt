#lang racket

(provide remove-complex-opera*)

(define (rco-arg arg)
  (if (pair? arg)
      (let ([s (gensym 'x.)])
        (values s
                `(,s ,(rco-exp arg))))
      (values arg '())))

(define (let-expansion exp bindings)
  (cond
   [(null? bindings) exp]
   [(null? (car bindings))
    (let-expansion exp (cdr bindings))]
   [else
    `(let (,(car bindings))
       ,(let-expansion exp (cdr bindings)))]))

(define (rco-exp e)
  (match e
    [`(let ([,id ,binding])
        ,exp)
     `(let ([,id ,(rco-exp binding)])
        ,(rco-exp exp))]
    [`(,op ,es ...)
     (define-values (ids bindings)
       (for/lists (ids bindings) ([e es])
         (rco-arg e)))
     (let-expansion (cons op ids)
                    bindings)]
    [else e]))

(define (remove-complex-opera* e)
  (match e
    [`(program ,info ,exp)
     `(program ,info ,(rco-exp exp))]))

(module+ test
  (require rackunit
           "test/test-programs.rkt"
           "interp.rkt")

  (for-each
   (lambda (program)
     (test-eq? "Transformation yields semantically equivilant R1 program"
               ((interp-R1 '()) (remove-complex-opera* program))
               ((interp-R1 '()) program)))
   R1-examples))
