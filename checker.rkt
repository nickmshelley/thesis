#lang racket/base
(require "completers/all-tokens.rkt"
         "word.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/system
         racket/string
         racket/port
         plot
         (only-in srfi/13 string-suffix?))

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
      (or success? (get-output-string error-string))))
  (define-values (passed messages) (partition boolean? res))
  #;(for-each (λ (message)
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
          (append (list (vector 'Passed remove-passed))
                  (sum-errors (results-wordsults remove)))
          #:skip 5 #:x-min 0 #:label "Remove")
         (discrete-histogram 
          (append (list (vector 'Passed truncate-passed))
                  (sum-errors (results-wordsults truncate)))
          #:skip 5 #:x-min 1 #:label "Truncate"
          #:color 2 #:line-color 2))
   (format "output/checker/~a.png" name)
   #:title (results-filename remove)
   #:x-label "Type"
   #:y-label "Amount"))

(define (sum-errors list-of-wordsults)
  (map vector
       '(Dup-Def Unbound-ID Not-Proc Contract-V Other)
       (apply map +
              (map (compose categorize-errors get-first-lines word-results-failed-messages)
                   list-of-wordsults))))

(define (get-first-lines str-list)
  (for/list ([str (in-list str-list)])
    (first (string-split str "\n"))))
(module+ test
  (check-equal? (get-first-lines (list (format "hi~nthere you~nguys")
                                       (format "another one~nhere")))
                (list "hi" "another one")))

(define (categorize-errors error-message-first-lines)
  (define-values (dup unbound not-proc contract other)
    (for/fold ([dup 0]
               [unbound 0]
               [not-proc 0]
               [contract 0]
               [other 0])
      ([message (in-list error-message-first-lines)])
      (cond
        [(string-suffix? "duplicate definition for identifier" message)
         (values (add1 dup) unbound not-proc contract other)]
        [(string-suffix? "unbound identifier in module" message)
         (values dup (add1 unbound) not-proc contract other)]
        [(string-suffix? "not a procedure;" message)
         (values dup unbound (add1 not-proc) contract other)]
        [(string-suffix? "contract violation" message)
         (values dup unbound not-proc (add1 contract) other)]
        [else
         (values dup unbound not-proc contract (add1 other))])))
  (list dup unbound not-proc contract other))
(module+ test
  (check-equal? (categorize-errors (list "lang: unbound identifier in module"
                                         "module: duplicate definition for identifier"
                                         "application: not a procedure;"
                                         "+: contract violation"
                                         "define: not allowed in an expression context"))
                (make-list 5 1)))

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
