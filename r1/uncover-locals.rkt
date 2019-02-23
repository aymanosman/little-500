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

;; covert into tests
;; '(seq (assign x.1 20)
;;       (seq (assign x.2 22)
;;            (seq (assign y (+ x.1 x.2))
;;                 (return y))))
