#lang racket/base
(require "completers/all-tokens.rkt"
         "word.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/system
         racket/string
         racket/port
         plot)

(module+ test
  (require rackunit))

;results : string list-of-word-results
(struct results (filename wordsults))
;word-results : word number list-of-string
(struct word-results (word passed failed-messages))
(module+ test
  (define (word-results->string wordsults)
    (format "(~a ~a ~a)"
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
                       (word-results->string wr1)
                       (word-results->string wr2))
               #f))))

(define (check-all-files files file-mod)
  (define percent 1)
  (map (λ (filename)
         (check-file file-mod filename percent))
       files))

; check-file : string -> results
(define (check-file file-mod filename percent)
  (define file-string (file->string filename))
  (define words (percent-of-words-from-file percent file-string))
  (results 
   (path->string filename)
   (for/list ([word words])
     (check-word word file-mod file-string))))

; check-word : word string -> word-results
; runs the programs resulting from replacing a word by all of its completions
(define (check-word word file-mod file-string)
  (define altered-string (file-mod file-string word))
  (define completions (get-completions altered-string ""))
  (define temp-file "/tmp/file.rkt")
  (define res
    (for/list ([completion completions])
      (with-output-to-file temp-file
        (lambda () 
          (display (replace-word-with-string word completion file-string)))
        #:exists 'replace)
      (define error-string (open-output-string)) ;will use this later
      (define success?
        (parameterize ([current-error-port error-string]
                       [current-output-port (open-output-nowhere)])
          (system (format "racket ~a" temp-file))))
      (printf "~n~n***********ERROR:~n~a*************~n~n" (get-output-string error-string))
      (or success? (get-output-string error-string))))
  (define-values (passed messages) (partition boolean? res))
  (for-each (λ (message)
              (printf "~n~a~n" message))
            messages)
  (word-results word (length passed) messages))
(module+ test
  (define str "#lang racket (define x 2) (+ x x) ;y")
  (define results (check-word (word "x" 29) string-w/o-word str))
  (check-true (word-results-equal?
               results
               (word-results (word "x" 29) 2 (make-list 5 "error"))))
  (set! str "#lang racket (define x 2) (+ x x) ;y")
  (set! results (check-word (word "x" 29) string-truncated-from-word str))
  (check-true (word-results-equal?
               results
               (word-results (word "x" 29) 2 (make-list 4 "error")))))

(define (replace-word-with-string to-replace to-insert text)
  (string-append (substring text 0 (word-pos to-replace))
                 to-insert
                 (substring text (+ (word-pos to-replace) 
                                    (string-length (word-str to-replace))))))
(module+ test
  (check-equal? (replace-word-with-string (word "adios" 8) 
                                          "goodbye" 
                                          "you say adios I say hello")
                "you say goodbye I say hello"))


(define (display-results remove truncate)
  (define name (string-replace (results-filename remove) "/" "_"))
  (define remove-passed (apply + (map word-results-passed (results-wordsults remove))))
  (define remove-failed (apply + (map (compose1 length word-results-failed-messages)
                                      (results-wordsults remove))))
  (define truncate-passed (apply + (map word-results-passed (results-wordsults truncate))))
  (define truncate-failed (apply + (map (compose1 length word-results-failed-messages)
                                        (results-wordsults truncate))))
  (plot-file 
   (list (discrete-histogram 
          (list (vector 'Passed remove-passed)
                (vector 'Failed remove-failed))
          #:skip 2.5 #:x-min 0 #:label "Remove")
         (discrete-histogram 
          (list (vector 'Passed truncate-passed)
                (vector 'Failed truncate-failed))
          #:skip 2.5 #:x-min 1 #:label "Truncate"
          #:color 2 #:line-color 2))
   (format "output/checker/~a.png" name)
   #:title (results-filename remove)
   #:x-label "Type"
   #:y-label "Amount"))

; add-results : results results -> results
(define (add-results r1 r2)
  (results "All" (append (results-wordsults r1) (results-wordsults r2))))

(module+ main
  (when (not (directory-exists? "output"))
    (make-directory "output"))
  (when (not (directory-exists? "output/checker"))
    (make-directory "output/checker"))
  (define remove 
    (check-all-files 
     (get-all-source-files "test-files/checker-source")
     string-w/o-word))
  (define truncate 
    (check-all-files 
     (get-all-source-files "test-files/checker-source")
     string-truncated-from-word))
  (for-each display-results remove truncate)
  (define remove-sum (foldl add-results
                            (results "All" empty)
                            remove))
  (define truncate-sum (foldl add-results
                              (results "All" empty)
                              truncate))
  (display-results remove-sum truncate-sum))
