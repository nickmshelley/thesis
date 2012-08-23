#lang racket/base

(require racket/match
         racket/list)

(provide string->words
         words->strings
         (struct-out word)
         word-equal?
         print-word)

(struct word (str pos))

(define (word-equal? w1 w2)
    (if (and (equal? (word-pos w1) (word-pos w2))
             (equal? (word-str w1) (word-str w2)))
        #t
        (begin (printf "word1: (~a ~a)~nword2: (~a ~a)"
                       (word-str w1) (word-pos w1)
                       (word-str w2) (word-pos w2))
               #f)))

(define (print-word a-word)
    (printf "(~a ~a)"
            (word-str a-word)
            (word-pos a-word)))

(define delimiters #<<@
\s\(\)\[\]",'`#|\\;
@
  )

; string->words : string -> list-of-word
; returns every sequence of non-delimiter characters, regardless of type
(define (string->words s)
  (map (match-lambda
         [(cons start end)
          (word (substring s start end) start)])
       (regexp-match-positions* (pregexp (format "[^~a]+" delimiters)) s)))
(module+ test
  (require rackunit)
  (define str "#lang racket (define \"hi there\" 5 + be) (hi there)")
  (define results (string->words str))
  (check-equal? (length results) 10)
  (check-true (word-equal? (first results) (word "lang" 1)))
  (check-true (word-equal? (second results) (word "racket" 6)))
  (check-true (word-equal? (third results) (word "define" 14)))
  (check-true (word-equal? (fourth results) (word "hi" 22)))
  (check-true (word-equal? (fifth results) (word "there" 25)))
  (check-true (word-equal? (sixth results) (word "5" 32)))
  (check-true (word-equal? (seventh results) (word "+" 34)))
  (check-true (word-equal? (eighth results) (word "be" 36)))
  (check-true (word-equal? (ninth results) (word "hi" 41)))
  (check-true (word-equal? (tenth results) (word "there" 44))))

; words->strings : list-of-word -> list-of-string
(define (words->strings words)
  (map word-str words))