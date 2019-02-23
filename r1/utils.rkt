#lang racket

(provide a-get
         a-set)

(define (a-get sym env)
  (cond
   [(null? env) #f]
   [(equal? (car (car env)) sym)
    (cdar env)]
   [else (a-get sym (cdr env))]))

(define (a-set sym val env)
  (cons `(,sym . ,val) env))

(module+ test
  (require rackunit)

  (test-case "Round trip of alist operations"
             (define new-env (a-set 'x 'y '()))
             (check-eq? 'y (a-get 'x new-env)))
  (test-case "Non-existent entry lookup"
             (check-false (a-get 'x '()))))
