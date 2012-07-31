#lang racket/base
(require "completers/all-tokens.rkt"
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

(define (check-all-files files)
  (map check-file files))

(define (check-file file)
  (define percent 1)
  (define file-string (file->string file))
  (define words (percent-of-words-from-file percent file-string))
  (results 
   file
   (for/list ([word words])
     (check-word word file-string))))

(define (check-word word file-string)
  (define holed-string (string-w/o-word file-string word))
  (define completions (get-completions holed-string 0 ""))
  (printf "word: ~a~nfile-string: ~a~nholed-string: ~a~ncompletions: ~a~n" (word-str word) file-string holed-string completions)
  (define temp-file "/tmp/file.rkt")
  (define res
    (for/list ([completion completions])
      (with-output-to-file temp-file
        (lambda () (insert-string completion holed-string (word-pos word)))
        #:exists 'replace)
      (define error-string (open-output-string)) ;will use this later
      (define success?
        (parameterize ([current-error-port error-string])
          (system (format "racket ~a" temp-file))))
      (or success? (get-output-string error-string))))
  (define-values (passed messages) (partition boolean? res))
  (word-results word (length passed) messages))

(define (insert-string to-insert holed-string pos)
  (string-append (substring holed-string 0 pos)
                 to-insert
                 (substring holed-string pos)))
(module+ test
  (check-equal? (insert-string "hello" "you say  I say goodbye" 8)
                "you say hello I say goodbye"))


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
