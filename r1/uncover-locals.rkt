#lang racket

(provide uncover-locals)

(define (uncover-locals e)
  (define (helper exp acc)
    (match exp
      [`(seq (assign ,x ,b) ,k)
       (define new-acc (cons x acc))
       (helper k new-acc)]
      [else (reverse acc)]))
  (match e
    [`(program ,info ,exp)
     `(program ((locals . ,(helper exp '())))
               ,exp)]))

(module+ test
  (require rackunit)

  (define example-c0
    '(program ()
              (seq (assign x.1 (+ 2 2))
                   (seq (assign x.2 38)
                        (seq (assign y (+ x.1 x.2))
                             (return y))))))

  (define get-locals cdaadr)

  (test-case
   "Compare locals collected by uncover-locals to predetermined list"
   (check-equal?
    (get-locals (uncover-locals example-c0))
    '(x.1 x.2 y))))
