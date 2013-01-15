#lang racket/base

(require "../word.rkt"
         racket/list)

(provide get-completions)

; get-completions : string string -> list-of-string
; returns all unique words in the string starting with prifix sorted alphabetically
(define (get-completions text-string prefix)
  (sort (filter
         (λ (word)
           (regexp-match (regexp-quote (string-append "^" prefix)) word))
         (remove-duplicates (words->strings (string->words text-string))))
        string<?))
(module+ test
  (require rackunit)
  (define test-str " a (all #all |there\\; [\"'allyour, \"(hi`]) \nthere ")
  (check-equal? (get-completions test-str "")
                '("a" "all" "allyour" "hi" "there"))
  (check-equal? (get-completions test-str "a")
                '("a" "all" "allyour"))
  (check-equal? (get-completions test-str "all")
                '("all" "allyour"))
  (check-equal? (get-completions test-str "b")
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
     (filter
      (λ (wrd/nest)
        (regexp-match (regexp-quote (string-append "^" prefix)) (word-str (word/nest-word wrd/nest))))
      (string->words/nest text-string))
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
  (require rackunit)
  (set! test-str "h (d (c) b (g f) e) a")
  (check-equal? (get-completions/nest test-str "" 0)
                '("a" "h" "b" "d" "e" "c" "f" "g"))
  (check-equal? (get-completions/nest test-str "" 12)
                '("c" "f" "g" "b" "d" "e" "a" "h"))
  (check-equal? (get-completions/nest test-str "b" 12)
                '("b")))