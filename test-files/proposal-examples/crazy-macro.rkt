#lang racket
(define-syntax (not-define stx)
 (syntax-case stx ()
   [(_ v)
    (with-syntax ([id (datum->syntax stx 'x)])
      (syntax
       (define id v)))]))

(not-define 6)
x

(let ()
 (not-define 7)

 x)

(define-for-syntax secret #f)
(require (for-syntax syntax/strip-context))
(define-syntax (macro1 stx)
 (syntax-case stx ()
   [(_ . exp)
    (let ([code (local-expand #'(begin . exp) 'expression '())])
      (printf "exp: ~s\n" #'exp)
      (printf "code: ~s\n" code)
      #`(begin (define #,(datum->syntax stx 'x)
                 #,(replace-context stx secret))
               #,code))]))
(define-syntax (macro2 stx)
 (syntax-case stx ()
   [(_ v)
    (begin (printf "Saving some stuff, brah\n")
           (set! secret #'v)
           #'(void))]))

(printf "~a\n" (macro2 8))

(let ()
 (macro1
  (macro2 9)
  (printf "Hey!\n")
  x))

(let ([y 31])
 (macro1
  (let ([y 32])
    (macro2 y))
  x))


(define-for-syntax secret2 #f)
(require (for-syntax syntax/strip-context))
(define-syntax (macro3 stx)
 (syntax-case stx ()
   [(_ . exp)
    (set! secret2 stx)
    #'(begin . exp)]))
(define-syntax (macro4 stx)
 (syntax-case stx ()
   [(_ v)
    (replace-context secret2 #'v)]))

(let ([y 9])
 (macro3
  (let ([y 10])
    (macro4 y))))
