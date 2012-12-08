#lang racket/base
(require "completers/all-tokens.rkt"
         "word.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/system
         racket/string
         racket/port
         racket/place
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

(define (check-all-files/places/remove files percent)
  (define num-workers (* 2 (processor-count)))
  (define places (for/list ([i (in-range num-workers)]
                            [file files])
                   (define p (make-worker-place/remove))
                   (place-channel-put p i)
                   (place-channel-put p percent)
                   p))
  (define result-messages (do-all-work-on-places files places))
  (map list->results result-messages))

(define (check-all-files/places/truncate files percent)
  (define num-workers (* 2 (processor-count)))
  (define places (for/list ([i (in-range num-workers)]
                            [file files])
                   (define p (make-worker-place/truncate))
                   (place-channel-put p i)
                   (place-channel-put p percent)
                   p))
  (define result-messages (do-all-work-on-places files places))
  (map list->results result-messages))

;; do-all-work-on-places : list-of-place-message list-of-place -> list-of-result-messages
;; number of messages has to be >= number of places
(define (do-all-work-on-places messages places)
  ; hand out initial messages to get all places working
  (for ([m messages]
        [p places])
    (place-channel-put p m))
  ; hand out remaining messages
  (define remaining-messages (list-tail messages (length places)))
  (define partial-answers 
    (pass-out-remaining-messages remaining-messages places))
  (define remaining-answers
    (wait-until-all-done places))
  (append partial-answers remaining-answers))

(define (pass-out-remaining-messages messages places)
  (let loop ([remaining messages] [answers empty])
      (if (empty? remaining)
          answers
          (apply sync
                 (for/list ([e (in-list places)])
                   (handle-evt e (lambda (ans)
                                   (place-channel-put e (first remaining))
                                   (loop (rest remaining) 
                                         (cons ans answers)))))))))


(define (wait-until-all-done evts)
  (let loop ([left evts] [answers empty])
    (if (empty? left)
        answers
        (apply sync
               (for/list ([e (in-list left)])
                 (handle-evt e (lambda (ans) 
                                 (loop (remove e left) 
                                       (cons ans answers)))))))))

(define (make-worker-place/remove)
  (place ch
    (define place-number (place-channel-get ch))
    (define percent (place-channel-get ch))
    (let loop ()
      (define file (place-channel-get ch))
      (define res (check-file/remove file percent place-number))
      (place-channel-put ch (results->list res))
      (loop))))

(define (make-worker-place/truncate)
  (place ch
    (define place-number (place-channel-get ch))
    (define percent (place-channel-get ch))
    (let loop ()
      (define file (place-channel-get ch))
      (define res (check-file/truncate file percent place-number))
      (place-channel-put ch (results->list res))
      (loop))))

; check-file : string -> results
(define (check-file/remove filename percent place-number)
  (define file-string (file->string filename))
  (define words (percent-of-words-from-file percent file-string))
  (results 
   (path->string filename)
   (for/list ([word words])
     (check-word word string-w/o-word file-string place-number))))

; check-file : string -> results
(define (check-file/truncate filename percent place-number)
  (define file-string (file->string filename))
  (define words (percent-of-words-from-file percent file-string))
  (results 
   (path->string filename)
   (for/list ([word words])
     (check-word word string-truncated-from-word file-string place-number))))

(define (list->results l)
  (results (first l) (map list->word-results (second l))))

(define (list->word-results l)
  (apply word-results l))

(define (results->list res)
  (list (results-filename res)
        (map word-results->list (results-wordsults res))))

(define (word-results->list wordsults)
  (list (word->list (word-results-word wordsults))
        (word-results-passed wordsults)
        (word-results-failed-messages wordsults)))

(define (word->list w)
  (list (word-str w) (word-pos w)))

(define (check-all-files files file-mod percent)
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
     (check-word word file-mod file-string 1))))

; check-word : word string -> word-results
; runs the programs resulting from replacing a word by all of its completions
(define (check-word word file-mod file-string place-number)
  (define altered-string (file-mod file-string word))
  (define completions (get-completions altered-string ""))
  (define temp-file (format "/tmp/file-~a.rkt" place-number))
  (define res
    (for/list ([completion completions])
      (with-output-to-file temp-file
        (lambda () 
          (display (replace-word-with-string word completion file-string)))
        #:exists 'replace)
      (define error-string (open-output-string))
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
  (define results (check-word (word "x" 29) string-w/o-word str 1))
  (check-true (word-results-equal?
               results
               (word-results (word "x" 29) 2 (make-list 5 "error"))))
  (set! str "#lang racket (define x 2) (+ x x) ;y")
  (set! results (check-word (word "x" 29) string-truncated-from-word str 1))
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
  (define percent 1)
  (when (not (directory-exists? "output"))
    (make-directory "output"))
  (when (not (directory-exists? "output/checker"))
    (make-directory "output/checker"))
  (define source-files (get-all-source-files "test-files/checker-source"))
  (define remove
    (check-all-files/places/remove source-files percent)
    #;(check-all-files source-files string-w/o-word percent))
  (define truncate
    (check-all-files/places/truncate source-files percent)
    #;(check-all-files source-files string-truncated-from-word percent))
  (for-each display-results remove truncate)
  (define remove-sum (foldl add-results
                            (results "All" empty)
                            remove))
  (define truncate-sum (foldl add-results
                              (results "All" empty)
                              truncate))
  (display-results remove-sum truncate-sum))
