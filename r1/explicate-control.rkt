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
     (exn `ec-tail "no matching clause" e)]
    ))

(define (ec-assign b x k)
  (match b
    ;; [(? symbol?)
    ;;  ]
    [(? fixnum?)
     `(seq (assign ,x ,b) ,k)]
    [`(let ([,x1 ,b1]) ,k1)
     (define new-k
       (ec-assign k1 x (ec-tail k)))
     (ec-assign b1 x1 new-k)]
    ;; TODO: edit to accomodate body
    ;; [`(let ([,x1 ,b1]) ,k1)
    ;;  (define new-k
    ;;    (ec-assign k1 x (ec-tail k)))
    ;;  (ec-assign b1 x1 new-k)]
    [`(,op ,es ...)
     `(seq (assign ,x ,b) ,(ec-tail k))]))

;; TODO: fix `ec-tail`, then write tests
(ec-tail '(let ([y (let ([x.1 (+ 2 2)])
                     something-else)])
            y))

(let ([y (let ([x.1 (+ 2 2)])
           x.1)])
  y)

;; (ec-tail '(let ([y (let ([x.1 42])
;;                      x.1)])
;;             y))

(ec-tail '(let ([y (let ([x.1 (+ 2 2)])
                     (let ([x.2 38])
                       (+ x.1 x.2)))])
            y))

(define (explicate-control e)
  (match e
    [`(program ,info ,exp)
     `(program ,info ,(ec-tail exp))]))
