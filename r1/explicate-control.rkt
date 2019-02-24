#lang racket

(provide explicate-control)

(define (ec-tail e)
  (match e
    [(? symbol?) `(return ,e)]
    [`(let ([,x ,b]) ,k)
     (define new-k (ec-tail k))
     (ec-assign b x new-k)]
    [`(,op ,es ...) e]
    [else
     (exn `ec-tail "no matching clause" e)]))

(define (ec-assign b x k)
  (match b
    [(? symbol?)
     `(seq (assign ,x ,b) ,k)]
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

;; TODO: delete
(explicate-control
 '(program ()
           (let ([x 40])
             (let ([y (+ 2 2)])
               (let ([x x])
                 (+ x y))))))

(define (explicate-control e)
  (match e
    [`(program ,info ,exp)
     `(program ,info ,(ec-tail exp))]))

(module+ test
  (require rackunit
           "test/test-programs.rkt")
  ;; TODO: replace with eq testing once c0-interp is complete
  (for-each
   (lambda (program)
     (test-pred "Transformation yields s-expr [placeholder]"
                pair? (explicate-control program)))
   R1-examples))
