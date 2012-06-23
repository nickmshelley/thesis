#lang racket

(provide get-completions)
(provide string->words)

(struct word (pos str))

(define delimiters #<<@
\s\(\)\[\]",'`#|\\;
@
  )

(define (string->words s)
  (map (match-lambda
         [(cons start end)
          (word start (substring s start end))])
       (regexp-match-positions* (pregexp (format "[^~a]+" delimiters)) s)))

(define (words->strings words)
  (map (λ (w) (word-str w)) words))

(define (get-completions text-string position prefix)
  (sort (filter
         (λ (word)
           (regexp-match (string-append "^" prefix) word))
         (remove-duplicates (words->strings (string->words text-string))))
        string<?))