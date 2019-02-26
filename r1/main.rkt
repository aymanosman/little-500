#lang racket

(require "uniquify.rkt"
         "remove-complex-opera.rkt"
         "explicate-control.rkt"
         "uncover-locals.rkt")

(provide compile-R1->C0)

(define compile-R1->C0
  (compose uncover-locals
           explicate-control
           remove-complex-opera*
           (uniquify '())))

(module+ test
  (require rackunit
           "interp.rkt"
           "test/test-programs.rkt")

  (for-each
   (lambda (program)
     (define compiled (compile-R1->C0 program))
     (test-eq? "Transformation to C0 preserves meaning"
               ((interp-C0 '()) compiled)
               ((interp-R1 '()) program)))
   R1-examples))
