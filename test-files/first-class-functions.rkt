#lang racket
(define (fun f)
  (f 3 2))

(fun +)
; returns 5
(fun make-vector)
; returns '#(2 2 2)

(define (return-fun a)
  (if (< a 5) + make-vector))

((return-fun 2) 5 2)
; returns 7
((return-fun 7) 5 2)
; returns '#(2 2 2 2 2)