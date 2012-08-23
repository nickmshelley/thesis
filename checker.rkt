#lang racket/base
(require "completers/all-tokens.rkt"
         "word.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/system)

(module+ test
  (require rackunit))

;results : string list-of-word-results
(struct results (filename wordsults))
;word-results : word number list-of-string
(struct word-results (word passed failed-messages))
(module+ test
  (define (print-word-results wordsults)
    (printf "(~a ~a ~a)"
            (print-word (word-results-word wordsults))
            (word-results-passed wordsults)
            (length (word-results-failed-messages wordsults))))
  (define (word-results-equal? wr1 wr2)
    (if (and (word-equal? (word-results-word wr1) (word-results-word wr2))
             (equal? (word-results-passed wr1) (word-results-passed wr2))
             (equal? (length (word-results-failed-messages wr1))
                     (length (word-results-failed-messages wr2))))
        #t
        (begin (printf "wordsults1: ~a~nwordsults2: ~a~n"
                       (print-word-results wr1)
                       (print-word-results wr2))
               #f))))

(define (check-all-files files)
  (map check-file files))

; check-file : string -> results
(define (check-file file)
  (define percent 1)
  (define file-string (file->string file))
  (define words (percent-of-words-from-file percent file-string))
  (results 
   file
   (for/list ([word words])
     (check-word word file-string))))

; check-word : word string -> word-results
; runs the programs resulting from replacing a word by all of its completions
(define (check-word word file-string)
  (define holed-string (string-w/o-word file-string word))
  (define completions (get-completions holed-string ""))
  (define temp-file "/tmp/file.rkt")
  (define res
    (for/list ([completion completions])
      (with-output-to-file temp-file
        (lambda () 
          (display (insert-string completion holed-string (word-pos word))))
        #:exists 'replace)
      (define error-string (open-output-string)) ;will use this later
      (define success?
        (parameterize ([current-error-port error-string])
          (system (format "racket ~a" temp-file))))
      (or success? (get-output-string error-string))))
  (define-values (passed messages) (partition boolean? res))
  (word-results word (length passed) messages))
(module+ test
  (define str "#lang racket (define x 2) (+ x x)")
  (define results (check-word (word "x" 29) str))
  (check-true (word-results-equal?
               results
               (word-results (word "x" 29) 2 (make-list 4 "error")))))

(define (insert-string to-insert holed-string pos)
  (string-append (substring holed-string 0 pos)
                 to-insert
                 (substring holed-string pos)))
(module+ test
  (check-equal? (insert-string "goodbye" "you say  I say hello" 8)
                "you say goodbye I say hello"))


(define (display-results res)
  (printf "Results for file: ~a~n"
          (results-filename res))
  (printf "Total passed: ~a~n"
          (apply + (map word-results-passed (results-wordsults res))))
  (printf "Total failed: ~a~n~n"
          (apply + (map (compose1 length word-results-failed-messages)
                        (results-wordsults res)))))

(module+ main
  (for-each display-results 
            (check-all-files 
             (get-all-source-files "test-files/checker-source"))))
