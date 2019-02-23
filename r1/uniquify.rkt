#lang racket

(require "utils.rkt")

(provide uniquify)

(define (uniquify* sym-table)
  (lambda (exp)
    (match exp
      [(? symbol?)
       (let ([new-sym (a-get exp sym-table)])
         (if new-sym new-sym exp))]
      [(? integer?) exp]
      [`(let ([,x ,e]) ,body)
       (let* ([new-sym (gensym 'x.)]
              [unq-body ((uniquify* (a-set x new-sym sym-table)) body)]
              [unq-e    ((uniquify* sym-table) e)])
         `(let ([,new-sym ,unq-e])
            ,unq-body))]
      [`(,op ,es ...)
       `(,op ,@(for/list ([e es])
                 ((uniquify* sym-table) e)))]
      [else (error "No matching clause" exp)])))

(define (uniquify sym-table)
  (lambda (p)
    (match p
      [`(program ,info ,e)
       `(program ,info ,((uniquify* sym-table) e))])))

(module+ test
  (require rackunit
           "test/test-programs.rkt"
           "interp.rkt")

  (define acontextual-uniquify (uniquify '()))

  (for-each
   (lambda (program)
     (test-eq? "Transformation preserves evaluation outcome"
               ((interp-R1 '()) (acontextual-uniquify program))
               ((interp-R1 '()) program)))
   R1-examples))
