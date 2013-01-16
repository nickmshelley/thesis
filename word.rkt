#lang racket/base

(require racket/match
         racket/list)

(provide string->words
         string->words/nest
         how-nested?
         words->strings
         (struct-out word)
         (struct-out word/nest))

(struct word (str pos) #:transparent)
(struct word/nest (word level) #:transparent)

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
  (define str "#lang racket (define \"hi there\" 5 + be) (hi (there))")
  (define results (string->words str))
  (check-equal? (length results) 10)
  (check-equal? (first results) (word "lang" 1))
  (check-equal? (second results) (word "racket" 6))
  (check-equal? (third results) (word "define" 14))
  (check-equal? (fourth results) (word "hi" 22))
  (check-equal? (fifth results) (word "there" 25))
  (check-equal? (sixth results) (word "5" 32))
  (check-equal? (seventh results) (word "+" 34))
  (check-equal? (eighth results) (word "be" 36))
  (check-equal? (ninth results) (word "hi" 41))
  (check-equal? (tenth results) (word "there" 45)))

; string->words : string -> list-of-word/nest
; returns every sequence of non-delimiter characters, regardless of type
(define (string->words/nest s)
  (define wrds (string->words s))
  (define first-word/nest
    (word/nest (first wrds)
               (how-nested? s (word-pos (first wrds)))))
  (cons first-word/nest
        (let loop ([words (rest wrds)]
                   [prev-word/nest first-word/nest])
          (cond
            [(empty? words)
             empty]
            [else
             (match-define (word str start) (first words))
             (define this-word/nest
               (word/nest (first words)
                          (+ (how-nested? (substring s (word-pos (word/nest-word prev-word/nest)) start)
                                          (- start (word-pos (word/nest-word prev-word/nest))))
                             (word/nest-level prev-word/nest))))
             (cons this-word/nest
                   (loop (rest words) this-word/nest))]))))
(module+ test
  (require rackunit)
  (set! str "#lang racket (define \"hi there\" 5 + be) (hi (there))")
  (set! results (string->words/nest str))
  (check-equal? (length results) 10)
  (check-equal? (first results) (word/nest (word "lang" 1) 0))
  (check-equal? (second results) (word/nest (word "racket" 6) 0))
  (check-equal? (third results) (word/nest (word "define" 14) 1))
  (check-equal? (fourth results) (word/nest (word "hi" 22) 1))
  (check-equal? (fifth results) (word/nest (word "there" 25) 1))
  (check-equal? (sixth results) (word/nest (word "5" 32) 1))
  (check-equal? (seventh results) (word/nest (word "+" 34) 1))
  (check-equal? (eighth results) (word/nest (word "be" 36) 1))
  (check-equal? (ninth results) (word/nest (word "hi" 41) 1))
  (check-equal? (tenth results) (word/nest (word "there" 45) 2)))

(define (how-nested? input-string offset)
  (define input (open-input-string input-string))
  (for/sum
   ([i (in-range offset)])
   (match (read-char input)
     [(? eof-object?)
      (error 'how-nested? "invalid offset ~e, file ended early" offset)]
     [#\(
      1]
     [#\)
      -1]
     [_
      0])))
(module+ test
  (require rackunit)
  (define sample "(a(b)c(d(e(f)g(h(i)j(k)l(m)n)o)p)q)r")
  (for ([m (in-list (regexp-match-positions* #rx"[a-z]" sample))]
        [a (in-list '(1 2 1 2 3 4 3 4 5 4 5 4 5 4 3 2 1 0))])
    (match-define (cons start end) m)
    (check-equal?
     (how-nested? sample start)
     a))
  (check-exn
   exn:fail?
   (λ ()
     (how-nested? sample +inf.0))))

; words->strings : list-of-word -> list-of-string
(define (words->strings words)
  (map word-str words))