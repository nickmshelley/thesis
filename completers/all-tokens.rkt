#lang racket/base

(require "../word.rkt"
         racket/list
         racket/function)

(provide get-completions
         get-completions/nest
         get-completions/keywords-and-position
         #;get-completions/awesome)

;; proximity<? : number word word -> boolean
;; returns #t if the absolute value of the difference between w1 and origin is less than that of w2 and origin
(define (proximity<? origin w1 w2)
  (< (abs (- (word-pos w1) origin))
     (abs (- (word-pos w2) origin))))

; get-completions : string string integer -> list-of-string
; returns all unique words in the string starting with prifix sorted alphabetically
; pos is ignored, but the functions have to take the same number of arguments
(define (get-completions filename text-string prefix pos)
  (prefix-filtered-strings
   prefix
   (remove-duplicates 
    (words->strings
     (sort (string->words text-string)
           (curry proximity<? pos))))))
(module+ test
  (require rackunit)
  (define test-str " a (all #all |there\\; [\"'allyour, \"(hi`]) \nthere ")
  (check-equal? (get-completions "name" test-str "" 14)
                '("there" "all" "allyour" "a" "hi"))
    (check-equal? (get-completions "name" test-str "" 0)
                '("a" "all" "there" "allyour" "hi"))
  (check-equal? (get-completions "name" test-str "a" 0)
                '("a" "all" "allyour"))
  (check-equal? (get-completions "name" test-str "all" 0)
                '("all" "allyour"))
  (check-equal? (get-completions "name" test-str "b" 0)
                '()))

; get-completions/nest : string string integer -> list-of-string
; returns all unique words in the string starting with prifix sorted alphabetically
(define (get-completions/nest filename text-string prefix pos)
  (define nested (how-nested? text-string pos))
  (remove-duplicates
   (map
    (λ (wrd/nest)
      (word-str (word/nest-word wrd/nest)))
    (sort 
     (prefix-filtered-strings
      prefix
      (string->words/nest text-string)
      #:selector (compose word-str word/nest-word))
     (λ (wn1 wn2)
       (define difference (- (abs (- nested (word/nest-level wn2)))
                             (abs (- nested (word/nest-level wn1)))))
       (cond
         [(positive? difference)
          #t]
         [(negative? difference)
          #f]
         [else
          (proximity<? pos (word/nest-word wn1) (word/nest-word wn2))]))))))
(module+ test
  (set! test-str "h (d (c) b (g f) e) a")
  (check-equal? (get-completions/nest "name" test-str "" 0)
                '("h" "a" "d" "b" "e" "c" "g" "f"))
  (check-equal? (get-completions/nest "name" test-str "" 12)
                '("g" "f" "c" "b" "e" "d" "a" "h"))
  (check-equal? (get-completions/nest "name" test-str "b" 12)
                '("b")))

(define (prefix-filtered-strings prefix strs #:selector [selector #f])
  (filter
   (λ (maybe-str)
     (define str
       (if selector
           (selector maybe-str)
           maybe-str))
     (regexp-match (string-append "^" (regexp-quote prefix)) str))
   strs))

;; get-completions/keywords-and-position : string string integer -> list-of-string
;; uses postion to see whether we are in a function application or argument position
;; uses keywords such as define to find possible functions and named values and ranks them higher
(define (get-completions/keywords-and-position filename text-string prefix pos)
  (define priority-completions
    ;; I can't think of a reason (or a good way) to distinguish between argument and other,
    ;; so I'm just lumping all non-function positions into the argument category
    (if (function-position? text-string pos)
        (get-functions text-string)
        empty))
  (remove-duplicates
   (append (sort (prefix-filtered-strings prefix priority-completions) string<?)
           (get-completions filename text-string prefix pos))))
(module+ test
  (check-equal? (get-completions/keywords-and-position "name" "( (define (zebra a) b)" "" 1)
                (list "zebra" "define" "a" "b"))
  (check-equal? (get-completions/keywords-and-position "name" "( (define (zebra a) b)" "" 2)
                (list "define" "zebra" "a" "b")))

;; function-position? : string integer -> bool
;; returns whether a given position is where a function application would go
(define (function-position? text-string pos)
  (and (> pos 0)
       (eq? (string-ref text-string (sub1 pos))
            #\()))
(module+ test
  (check-equal? (function-position? "(hi there)" 0)
                #f)
  (check-equal? (function-position? "(hi there)" 1)
                #t)
  (check-equal? (function-position? "(hi there)" 2)
                #f))

;; get-functions : string -> list-of-string
;; gets all function names out of a text string
(define (get-functions text-string)
  (flatten
   (append
    (regexp-match* #px"\\(define[\\s]\\(([^\\s\\)]+)" text-string #:match-select cdr)
    (regexp-match* #px"\\(let[\\s]+([^(]+)[\\s]+\\([\\S]+\\)" text-string #:match-select cdr))))
(module+ test
  (check-equal? (get-functions "(define (all b) c) (let good (d) e) (define (boys) f) (let go () g) (define bad h) (let ([x 5]) x)")
                (list "all" "boys" "good" "go")))