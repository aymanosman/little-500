#lang racket

(provide R1-examples)

;; TODO: add descriptions; transform to alist
(define R1-examples
  '((program ()
             (let ([x 42]) x))

    (program ()
             (+ 2 2 38))

    (program ()
             (let ([x (let ([x 42])
                        x)])
               x))

    (program ()
             (let ([x 42])
               (let ([y 2])
                 (let ([x x])
                   x))))

    (program ()
             (let ([x 40])
               (let ([y 2])
                 (let ([x x])
                   (+ x y)))))

    (program ()
             (let ([x (let ([x 42])
                        x)])
               x))

    (program ()
             (let ([x (let ([x 42])
                        x)])
               x))

    (program ()
             (let ([x 10])
               (let ([y (let ([x x])
                          (+ x x))])
                 (let ([x x])
                   (+ x y 12)))))

    (program ()
             (+ 48 (- (+ 2 2) 10)))

    (program ()
             (+ 45 (- (+ (let ([y (+ 20 (- 15))])
                           y) 2)
                      10)))))
