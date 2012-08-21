#lang racket

(provide get-completions
         string->words
         (struct-out word))

(module+ test
  (require rackunit))

(struct word (str pos))

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
  (define (word-equal? w1 w2)
    (if (and (equal? (word-pos w1) (word-pos w2))
             (equal? (word-str w1) (word-str w2)))
        #t
        (begin (printf "word1: (~a ~a)~nword2: (~a ~a)"
                       (word-str w1) (word-pos w1)
                       (word-str w2) (word-pos w2))
               #f)))
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

; get-completions : string string -> list-of-string
; returns all unique words in the string starting with prifix sorted alphabetically
(define (get-completions text-string prefix)
  (sort (filter
         (λ (word)
           (regexp-match (string-append "^" prefix) word))
         (remove-duplicates (words->strings (string->words text-string))))
        string<?))
(module+ test
  (define test-str " a (all #all |there\\; [\"'allyour, \"(hi`]) \nthere ")
  (check-equal? (get-completions test-str "")
                '("a" "all" "allyour" "hi" "there"))
  (check-equal? (get-completions test-str "a")
                '("a" "all" "allyour"))
  (check-equal? (get-completions test-str "all")
                '("all" "allyour"))
  (check-equal? (get-completions test-str "b")
                '()))