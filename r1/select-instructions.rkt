#lang racket

(define (se-arg exp)
  (match exp
    [(? fixnum?) `(int ,exp)]
    [(? symbol?) `(var ,exp)]
    [else (exn `se-arg "no case for" exp)]))

(define (se-stmt stmt)
  (match stmt
    [`(assign ,var ,e)
     (match e
       ;; NOTE: limited to two operands for now
       [`(+ ,e1 ,e2)
        (cond
         [(eq? var e1)
          `((addq ,(se-arg e2) ,(se-arg var)))]
         [(eq? var e2)
          `((addq ,(se-arg e1) ,(se-arg var)))]
         [else
          `((movq ,(se-arg e1) ,(se-arg var))
            (addq ,(se-arg e2) ,(se-arg var)))])]
       [`(- ,e1)
        (if (eq? var e1)
            `((negq ,(se-arg var)))
            `((movq ,(se-arg e1) ,(se-arg var))
              (negq ,(se-arg var))))]
       [(? fixnum?)
        `((movq ,(se-arg e) ,(se-arg var)))]
       [(? symbol?)
        `((movq ,(se-arg e) ,(se-arg var)))]
       [else (exn `se-stmt "no case found for" exp)])]
    [else (exn `se-stmt "no case found for" stmt)]))

(define (se-tail tail)
  (match tail
    ;; NOTE: may be simplistic...
    [`(return ,exp)
     `((movq ,(se-arg exp) (reg rax))
       (jmp conclusion))]
    [`(seq ,stmt ,rest)
     (append (se-stmt stmt)
             (se-tail rest))]
    [else (exn `se-tail "no case found for" tail)]))

(define (select-instructions p)
  (match p
    [`(program ,info ,e)
     (se-tail e)]
    [else (exn `select-instructions "no matching case for" p)]))

;; TODO: add higher-level tests
(module+ test
  (require rackunit)

  (check-equal?
   (se-stmt '(assign x.1 (+ 2 40)))
   '((movq (int 2) (var x.1)) (addq (int 40) (var x.1))))

  (check-equal?
   (se-stmt '(assign x.1 (+ x.1 40)))
   '((addq (int 40) (var x.1))))

  (check-equal?
   (se-stmt '(assign x.1 (+ 40 x.1)))
   '((addq (int 40) (var x.1))))

  (check-equal?
   (se-stmt '(assign x.1 (- 40)))
   '((movq (int 40) (var x.1)) (negq (var x.1))))

  (check-equal?
   (se-stmt '(assign x.1 (- x.1)))
   '((negq (var x.1))))

  (check-equal?
   (se-stmt '(assign x.1 42))
   '((movq (int 42) (var x.1))))

  (check-equal?
   (se-stmt '(assign x.1 y.1))
   '((movq (var y.1) (var x.1)))))
