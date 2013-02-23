#lang racket/base

(require "../word.rkt"
         racket/list)

(provide get-completions
         get-completions/nest
         get-completions/keywords-and-position)

; get-completions : string string integer -> list-of-string
; returns all unique words in the string starting with prifix sorted alphabetically
; pos is ignored, but the functions have to take the same number of arguments
(define (get-completions text-string prefix pos)
  (sort (prefix-filtered-strings
         prefix
         (remove-duplicates (words->strings (string->words text-string))))
        string<?))
(module+ test
  (require rackunit)
  (define test-str " a (all #all |there\\; [\"'allyour, \"(hi`]) \nthere ")
  (check-equal? (get-completions test-str "" 0)
                '("a" "all" "allyour" "hi" "there"))
  (check-equal? (get-completions test-str "a" 0)
                '("a" "all" "allyour"))
  (check-equal? (get-completions test-str "all" 0)
                '("all" "allyour"))
  (check-equal? (get-completions test-str "b" 0)
                '()))

; get-completions/nest : string string integer -> list-of-string
; returns all unique words in the string starting with prifix sorted alphabetically
(define (get-completions/nest text-string prefix pos)
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
          (string<? (word-str (word/nest-word wn1))
                    (word-str (word/nest-word wn2)))]))))))
(module+ test
  (set! test-str "h (d (c) b (g f) e) a")
  (check-equal? (get-completions/nest test-str "" 0)
                '("a" "h" "b" "d" "e" "c" "f" "g"))
  (check-equal? (get-completions/nest test-str "" 12)
                '("c" "f" "g" "b" "d" "e" "a" "h"))
  (check-equal? (get-completions/nest test-str "b" 12)
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
(define (get-completions/keywords-and-position text-string prefix pos)
  (define priority-completions
    ;; I can't think of a reason (or a good way) to distinguish between argument and other,
    ;; so I'm just lumping all non-function positions into the argument category
    (if (function-position? text-string pos)
        (get-functions text-string)
        empty))
  (remove-duplicates
   (append (sort (prefix-filtered-strings prefix priority-completions) string<?)
           (get-completions text-string prefix pos))))
(module+ test
  (check-equal? (get-completions/keywords-and-position "( (define (zebra a) b)" "" 1)
                (list "zebra" "a" "b" "define"))
  (check-equal? (get-completions/keywords-and-position "( (define (zebra a) b)" "" 2)
                (list "a" "b" "define" "zebra")))

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
    (regexp-match* #px"\\(let[\\s](\\S+)" text-string #:match-select cdr))))
(module+ test
  (check-equal? (get-functions "(define (all b) c) (let good (d) e) (define (boys) f) (let go () g) (define bad h)")
                (list "all" "boys" "good" "go")))