#lang racket/base
(require "completers/all-tokens.rkt"
         "completers/macros.rkt"
         "completers/bytecode.rkt"
         "word.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/system
         racket/string
         racket/port
         racket/place
         racket/sandbox
         racket/pretty
         plot
         (only-in srfi/13 string-suffix?))

(module+ test
  (require rackunit))

(module+ main
  #;(define methods '(naive nest keywords macros bytecode combined))
  (define methods '(macros))
  (unless (directory-exists? "output")
    (make-directory "output"))
  (unless (directory-exists? "output/checker")
    (make-directory "output/checker"))
  (for ([method (in-list methods)])
    (define dir (string-append "output/checker/" (symbol->string method)))
    (unless (directory-exists? dir)
      (make-directory dir))
    (test-with-method method)))

(define (test-with-method method)
  (printf "TESTING WITH ~a~n" method)
  (define percent .1)
  (define dir-base "test-files/packages")
  (define sub-dirs '("frog" "marketplace" "pfds"))
  (define top-output-dir (build-path "output/checker" (symbol->string method)))
  (define-values (remove-sum truncate-sum)
    (for/fold ([remove-total-sum (results "All" empty)]
               [truncate-total-sum (results "All" empty)])
      ([dir (in-list sub-dirs)])
      (define test-dir (build-path dir-base dir))
      (define output-dir (build-path top-output-dir dir))
      (unless (directory-exists? output-dir)
        (make-directory output-dir))
      (define source-files (get-all-source-files test-dir))
      (pretty-print source-files)
      (printf "REMOVE~n")
      (define remove
        (check-all-files/places 'remove method source-files percent))
      (printf "TRUNCATE~n")
      (define truncate
        (check-all-files/places 'truncate method source-files percent))
      (for-each display-results remove truncate (make-list (length remove) output-dir))
      (define remove-sum (foldl add-results
                                (results "All" empty)
                                remove))
      (define truncate-sum (foldl add-results
                                  (results "All" empty)
                                  truncate))
      (display-results remove-sum truncate-sum output-dir)
      (values (add-results remove-total-sum remove-sum)
              (add-results truncate-total-sum truncate-sum))))
  (display-results remove-sum truncate-sum top-output-dir))

;results : string list-of-word-results
(struct results (filename wordsults) #:transparent)

;word-results : word number list-of-string
(struct word-results (word passed failed-messages) #:transparent)

(module+ test
  (define (word-results->string wordsults)
    (format "(~a ~a ~a)"
            (word-results-word wordsults)
            (word-results-passed wordsults)
            (length (word-results-failed-messages wordsults))))
  (define (word-results-equal? wr1 wr2)
    (if (and (equal? (word-results-word wr1) (word-results-word wr2))
             (equal? (word-results-passed wr1) (word-results-passed wr2))
             (equal? (length (word-results-failed-messages wr1))
                     (length (word-results-failed-messages wr2))))
        #t
        (begin (printf "wordsults1: ~a~nwordsults2: ~a~n"
                       (word-results->string wr1)
                       (word-results->string wr2))
               #f))))

;; check-all-files/places : symbol sybmol list-of-string float
(define (check-all-files/places file-mod completion files percent)
  (define num-workers (* 1 (processor-count)))
  (define places (for/list ([i (in-range num-workers)]
                            [file files])
                   (define p (make-worker-place))
                   (place-channel-put p (vector file-mod completion i percent))
                   p))
  ; hand out initial messages to get all places working
  (for ([f files]
        [p places])
    (place-channel-put p f))
  (define result-messages (do-all-work-on-places 
                           (list-tail files (length places))
                           places
                           empty))
  (printf "GETTING RESULTS~n")
  (map list->results result-messages))

;; do-all-work-on-places : list-of-place-message list-of-place -> list-of-result-messages
;; places should have already received their initial messages
(define (do-all-work-on-places messages places results-so-far)
  (cond
    [(and (empty? messages) (empty? places))
     results-so-far]
    [else
     (apply
      sync
      (for/list ([p (in-list places)])
        (handle-evt
         p
         (λ (answer)
           (cond
             [(empty? messages)
              (place-channel-put p #f)
              (do-all-work-on-places empty (remove p places) (cons answer results-so-far))]
             [else
              (place-channel-put p (first messages))
              (do-all-work-on-places (rest messages) places (cons answer results-so-far))])))))]))

(require racket/match)
(define (make-worker-place)
  (place ch
    (match-define (vector file-mod-id completion-id place-number percent)
      (place-channel-get ch))
    (define file-mod-f
      (if (eq? file-mod-id 'remove)
          string-w/o-word
          string-truncated-from-word))
    (define completion-f
      (cond
        [(eq? completion-id 'naive)
         get-completions]
        [(eq? completion-id 'nest)
         get-completions/nest]
        [(eq? completion-id 'keywords)
         get-completions/keywords-and-position]
        [(eq? completion-id 'macros)
         get-macro-completions]
        [(eq? completion-id 'bytecode)
         get-zo-completions]
        [(eq? completion-id 'combined)
         (λ (fn txt prefix pos)
           (remove-duplicates
            (append (get-completions/nest fn txt prefix pos)
                    (get-zo-completions fn txt prefix pos))))]
        [else
         (error "Unknown method:" completion-id)]))
    (let loop ()
      (define file (place-channel-get ch))
      (when file
        (define res
          (cond
            [(equal? file "test-files/packages/marketplace/support/gui.rkt")
             (results (path->string file) (list (word-results (word "" -1) 0 empty)))]
            [else 
             (check-file file file-mod-f completion-f percent place-number)]))
        (place-channel-put ch (results->list res))
        (loop)))))

; check-file : string symbol symbol integer integer -> results
(define (check-file filename file-mod completion-f percent place-number)
  (printf "Checking ~a...~n" filename)
  (define file-string (file->string filename))
  (define words (percent-of-words-from-file percent file-string))
  (results 
   (path->string filename)
   (for/list ([word words])
     (check-word word (path->string filename) file-mod completion-f file-string place-number))))

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

; check-word : word string -> word-results
; runs the programs resulting from replacing a word by all of its completions
(define (check-word word filename file-mod completion-f file-string place-number)
  (define altered-string (file-mod file-string word))
  (define completions (completion-f filename altered-string "" (word-pos word)))
  (define-values (base fn _) (split-path filename))
  (define temp-file (build-path base (format "nicks-thesis-file-~a.nick" place-number)))
  (define res
    (for/list ([completion completions]
               [i (in-range 5)])
      (with-output-to-file temp-file
        (lambda () 
          (display (replace-word-with-string word completion file-string)))
        #:exists 'replace)
      (define-values (proc in out err)
        (subprocess #f #f #f (find-executable-path "racket") temp-file))
      (define result (sync/timeout 5 proc))
      (define success?
        (if result
            (= (subprocess-status result) 0)
            (begin
              (subprocess-kill proc #t)
              #f)))
      (define error-string (port->string err))
      (close-input-port in)
      (close-output-port out)
      (close-input-port err)
      (delete-file temp-file)
      (or success? error-string)))
  (define-values (passed messages) (partition boolean? res))
  #;(for-each (λ (message)
                (when (> (length (string-split message "cannot open module file")) 1)
                  (printf "MESSAGE:~n~a~n" message)))
              messages)
  (word-results word (length passed) messages))
(module+ test
  (define str "#lang racket (define x 2) (+ x x) ;y")
  (define results (check-word (word "x" 29) string-w/o-word get-completions str 1))
  (check-true (word-results-equal?
               results
               (word-results (word "x" 29) 2 (make-list 5 "error"))))
  (set! str "#lang racket (define x 2) (+ x x) ;y")
  (set! results (check-word (word "x" 29) string-truncated-from-word get-completions str 1))
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


(define (display-results remove truncate output-dir)
  (define name (string-replace (results-filename remove) "/" "_"))
  (define remove-passed (apply + (map word-results-passed (results-wordsults remove))))
  (define remove-failed (really-sum-errors (flatten (map (compose get-first-lines word-results-failed-messages) (results-wordsults remove)))))
  (define truncate-passed (apply + (map word-results-passed (results-wordsults truncate))))
  (define truncate-failed (really-sum-errors (flatten (map (compose get-first-lines word-results-failed-messages) (results-wordsults truncate)))))
  (define remove-write `((passed . ,remove-passed) (failed . ,remove-failed)))
  (define truncate-write `((passed . ,truncate-passed) (failed . ,truncate-failed)))
  (define prefix (format "~a/~a" output-dir name))
  (with-output-to-file (format "~a.txt" prefix)
    (λ () (pretty-print `((remove . ,remove-write) (truncate . ,truncate-write))))
    #:exists 'replace)
  (plot-file 
   (list (discrete-histogram 
          (append (list (vector 'Passed remove-passed))
                  (sum-errors (results-wordsults remove)))
          #:skip 5 #:x-min 0 #:y-min .1 #:label "Remove")
         (discrete-histogram 
          (append (list (vector 'Passed truncate-passed))
                  (sum-errors (results-wordsults truncate)))
          #:skip 5 #:x-min 1 #:y-min .1 #:label "Truncate"
          #:color 2 #:line-color 2))
   (format "~a.png" prefix)
   #:title (results-filename remove)
   #:x-label "Type"
   #:y-label "Amount"))

(define (really-sum-errors error-message-first-lines)
  (define single-errors (remove-duplicates error-message-first-lines))
  (cons `(total . ,(length error-message-first-lines))
        (sort 
         (for/list ([error-message (in-list single-errors)])
           `(,error-message . ,(count (λ (arg) (equal? arg error-message))
                                      error-message-first-lines)))
         >
         #:key cdr)))

(define (sum-errors list-of-wordsults)
  (map vector
       '(Dup-Def Unbound-ID Not-Proc Contract-V Other)
       (apply map +
              (map (compose categorize-errors get-first-lines word-results-failed-messages)
                   list-of-wordsults))))

(define (get-first-lines str-list)
  (for/list ([str (in-list str-list)]
             #:when (> (string-length str) 0))
    (string-trim 
     (last (string-split
            (first (string-split str "\n"))
            ":")))))
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
