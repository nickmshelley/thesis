#lang racket/base

(require "../word.rkt"
         racket/list)

(provide get-completions)

; get-completions : string string -> list-of-string
; returns all unique words in the string starting with prifix sorted alphabetically
(define (get-completions text-string prefix)
  (sort (filter
         (λ (word)
           (regexp-match (string-append "^" prefix) word))
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