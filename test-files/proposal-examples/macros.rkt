#lang racket
(define-syntax-rule (while test body)
  (let loop ()
    (when test body (loop))))

(define val 0)
(while (< val 10)
       (begin
         (print val)
         (set! val (add1 val))))

(define-syntax-rule (swap a b)
  (let ([tmp a])
    (set! a b)
    (set! b tmp)))

(define a 1)
(define b 2)
(list a b)
(swap a b)
(list a b)

(define-syntax my-if
  (syntax-rules ()
    [(my-if test then) (if test then (void))]
    [(my-if test then else) (if test then else)]))

(my-if #f "hi")
(my-if #f "hi" "there")

(define-syntax and
  (syntax-rules ()
    [(and a) a]
    [(and a b) (if a b #f)]
    [(and a b c ...) (if a (and b c ...) #f)]))

(and #f #t #t #t #t)
    