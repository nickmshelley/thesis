#lang racket/base
(require "completers/all-tokens.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/function
         racket/generator)

(module+ test
  (require rackunit))

; A rankings combines a filename with analyzed stats
; Filename : string
; ranked : list of number - index is rank and number is how many of that rank
; unranked : number
; missed : number
(struct rankings (filename ranked unranked missed))

(module+ test
  ;rankings-equal? : rankings rankings -> boolean
  (define (rankings-equal? r1 r2)
    (and (equal? (rankings-filename r1) (rankings-filename r2))
         (equal? (rankings-ranked r1) (rankings-ranked r2))
         (equal? (rankings-unranked r1) (rankings-unranked r2))
         (equal? (rankings-missed r1) (rankings-missed r2)))))

; test-all-files : list-of-filepath (string word -> string) -> list-of-rankings
(define (test-all-files files file-mod)
  (map (curry test-file file-mod) files))

; test-file : (string word -> string) string -> rankings
; file-mod modifies the string source for autocompletion at that point
; returns list of ranks in completion list
(define (test-file file-mod filename)
  (define percent 1)
  (define file-string (file->string filename))
  (define words (percent-of-words-from-file percent file-string))
  (define results 
    (for/list ([word words])
      (define completions (get-completions (file-mod file-string word) 0 ""))
      (define result (member (word-str word) completions))
      (and result
           (- (length completions) (length result)))))
  (define-values (ranked unranked missed) (analyze results))
  (rankings filename ranked unranked missed))

; analyze : string list-of-number -> (values list-of-number number number)
(define (analyze results)
  (define hits-list (filter identity results))
  (define threshold 5)
  (values (for/list ([i (in-range threshold)])
            (count (λ (num) (= num i)) hits-list))
          (count (λ (num) (>= num threshold)) hits-list)
          (count not results)))
(module+ test
  (define-values (ranked unranked missed)
    (analyze (list 6 6 4 4 2 1 0 0 5 8 9 #f #f)))
  (check-equal? ranked (list 2 1 1 0 2))
  (check-equal? unranked 5)
  (check-equal? missed 2))

(define (display-rankings ranks)
  (define ranked (rankings-ranked ranks))
  (define unranked (rankings-unranked ranks))
  (define missed (rankings-missed ranks))
  (printf "Results for file: ~a~n" 
          (rankings-filename ranks))
  (printf "Total completed tokens: ~a~n" 
          (apply + unranked missed ranked))
  (printf "Ranks of correct completions:~n")
  (for ([num ranked]
        [i (in-range (length ranked))])
    (printf "~a: ~a~n" (add1 i) num))
  (printf ">~a: ~a~n" (length ranked) unranked)
  (printf "Not included: ~a~n~n" missed))

(module+ main
  (printf "Alter file method: Remove~n~n")
  (for-each display-rankings 
            (test-all-files 
             (get-all-source-files "test-files") 
             string-w/o-word))
  (printf "Alter file method: Truncate~n~n")
  (for-each display-rankings 
            (test-all-files 
             (get-all-source-files "test-files") 
             string-truncated-from-word)))