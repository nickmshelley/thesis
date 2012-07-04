#lang racket
(define (return-something a)
  (if (< a 5) 10 "hi"))

(define num (return-something 2))
; num is 10
(set! num (return-something 8))
; num is "hi"

(define (add2 a)
  (+ a 2)) ;is a actually a number?

(add2 #t)
; error