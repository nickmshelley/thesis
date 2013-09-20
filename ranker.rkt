#lang racket/base
(require "completers/all-tokens.rkt"
         "completers/macros.rkt"
         "completers/bytecode.rkt"
         "word.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/function
         racket/string
         plot)

(module+ main
  (define methods '(naive nest keywords macros bytecode))
  (unless (directory-exists? "output")
    (make-directory "output"))
  (unless (directory-exists? "output/ranker")
    (make-directory "output/ranker"))
  (for ([method (in-list methods)])
    (define dir (string-append "output/ranker/" (symbol->string method)))
    (unless (directory-exists? dir)
      (make-directory dir))
    (test-with-method method)))

;; test-with-method : symbol -> void
(define (test-with-method method)
  (define completion-f
    (cond
      [(eq? method 'naive)
       get-completions]
      [(eq? method 'nest)
       get-completions/nest]
      [(eq? method 'keywords)
       get-completions/keywords-and-position]
      [(eq? method 'macros)
       get-macro-completions]
      [(eq? method 'bytecode)
       get-zo-completions]
      [else
       (error "Unknown method:" method)]))
  (define remove 
    (test-all-files 
     (get-all-source-files "test-files") 
     string-w/o-word
     completion-f))
  (define truncate
    (test-all-files 
     (get-all-source-files "test-files") 
     string-truncated-from-word
     completion-f))
  (for-each display-rankings remove truncate (make-list (length remove) method))
  (define remove-sum (foldl add-rankings 
                            (rankings "All" (make-list 5 0) 0 0)
                            remove))
  (define truncate-sum (foldl add-rankings
                              (rankings "All" (make-list 5 0) 0 0)
                              truncate))
  (display-rankings remove-sum truncate-sum method))

; A rankings combines a filename with analyzed stats
; Filename : string
; ranked : list of number - index is rank and number is how many of that rank
; unranked : number
; missed : number
(struct rankings (filename ranked unranked missed) #:transparent)
(module+ test
  (require rackunit))

; test-all-files : list-of-filepath (string word -> string) -> list-of-rankings
(define (test-all-files files file-mod completion-f)
  (define percent 1)
  (map (λ (filename)
         (test-file file-mod
                    completion-f
                    (path->string filename)
                    (file->string filename)
                    percent))
       files))

; test-file : (string word -> string) string -> rankings
; file-mod modifies the string source for autocompletion at that point
; returns list of ranks in completion list
(define (test-file file-mod completion-f filename file-string percent)
  (define words (percent-of-words-from-file percent file-string))
  (define results 
    (for/list ([word words])
      (define completions (completion-f filename (file-mod file-string word) "" (word-pos word)))
      (define result (member (word-str word) completions))
      (and result
           (- (length completions) (length result)))))
  (define-values (ranked unranked missed) (analyze results))
  (rankings filename ranked unranked missed))
(module+ test
  ;all completions: (+ 5 a be c define hi lang racket there you)
  (define str "#lang racket be (define you \"hi there you\" 5 + be) (define + there you) a c")
  (define rank1 (test-file string-w/o-word get-completions "name" str 1))
  (define-values (ranked unranked missed)
    ;these results can be confusing because only symbols as defined by string->word-symbols are tested
    ;but completions gets all words as defined by string->words
    (analyze (list 3 5 10 0 3 5 0 9 10 #f #f)))
  (check-equal? rank1 (rankings "name" ranked unranked missed))
  (define rank2 (test-file string-truncated-from-word get-completions "name" str 1))
  (set!-values (ranked unranked missed)
    ;remember that the positions change with every word (since later words like a and c get truncated)
    (analyze (list #f #f #f #f 2 3 0 7 8 #f #f)))
  (check-equal? rank2 (rankings "name" ranked unranked missed)))

; analyze : string list-of-number -> (values list-of-number number number)
(define (analyze results)
  (define hits-list (filter identity results))
  (define threshold 5)
  (values (for/list ([i (in-range threshold)])
            (count (λ (num) (= num i)) hits-list))
          (count (λ (num) (>= num threshold)) hits-list)
          (count not results)))
(module+ test
  (set!-values (ranked unranked missed)
    (analyze (list 6 6 4 4 2 1 0 0 5 8 9 #f #f)))
  (check-equal? ranked (list 2 1 1 0 2))
  (check-equal? unranked 5)
  (check-equal? missed 2))

; display-rankings : list-of-rankings list-of-rankings -> void
; plots raknings for each file side by side
(define (display-rankings remove truncate method)
  (define name (string-replace (rankings-filename remove) "/" "_"))
  (plot-file 
   (list (discrete-histogram
          (append (ranked->vectors (rankings-ranked remove))
                  (list (vector 'Unranked (rankings-unranked remove)))
                  (list (vector 'Missed (rankings-missed remove))))
          #:skip 2.5 #:x-min 0 #:label "Remove")
         (discrete-histogram
          (append (ranked->vectors (rankings-ranked truncate))
                  (list (vector 'Unranked (rankings-unranked truncate)))
                  (list (vector 'Missed (rankings-missed truncate))))
          #:skip 2.5 #:x-min 1 #:label "Truncate"
          #:color 2 #:line-color 2))
   (format "output/ranker/~a/~a.png" (symbol->string method) name)
   #:title (rankings-filename remove)
   #:x-label "Rank"
   #:y-label "Amount"))

; ranked->vectors : list-of-number -> list-of #(number number)
(define (ranked->vectors ranked)
  (for/list ([i (in-range 1 (add1 (length ranked)))]
             [num ranked])
    (vector i num)))

; sum-rankings : rankings rankings -> rankings
; adds two rankings together
(define (add-rankings r1 r2)
  (rankings "All" 
            (map + (rankings-ranked r1)
                 (rankings-ranked r2))
            (+ (rankings-unranked r1)
               (rankings-unranked r2))
            (+ (rankings-missed r1)
               (rankings-missed r2))))