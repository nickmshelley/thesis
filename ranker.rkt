#lang racket/base
(require "completers/all-tokens.rkt"
         racket/list
         racket/file
         racket/function
         srfi/13)

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

; get-all-source-files : path-string -> list-of-filepath
; gets all racket files in directory
(define (get-all-source-files dir)
  (for/list ([path (in-directory dir)]
             #:when (and (file-exists? path)
                         (or (string-suffix? ".rkt" (path->string path))
                             (string-suffix? ".ss" (path->string path)))))
    path))

; test-all-files : list-of-filepath (string word -> string) -> list-of-rankings
(define (test-all-files files file-mod)
  (map (compose1 (curry test-file file-mod)) files))

(define (string-w/o-word s w)
  (string-append (substring s 0 (word-pos w))
                 (substring s (+ (word-pos w) (string-length (word-str w))))))
(module+ test
  (require rackunit)
  (define test-string "(this is) a \"test\"")
  (define words (string->words test-string))
  (check-equal? (string-w/o-word test-string (third words))
                "(this is)  \"test\"")
  (check-equal? (string-w/o-word test-string (first words))
                "( is) a \"test\"")
  (check-equal? (string-w/o-word test-string (fourth words))
                "(this is) a \"\""))

(define (string-truncated-from-word s w)
  (substring s 0 (word-pos w)))
(module+ test
  (check-equal? (string-truncated-from-word test-string (third words))
                "(this is) ")
  (check-equal? (string-truncated-from-word test-string (first words))
                "(")
  (check-equal? (string-truncated-from-word test-string (fourth words))
                "(this is) a \""))

(define (list-random-ref l)
  (list-ref l (random (length l))))

; test-file : (string word -> string) string -> rankings
; file-mod modifies the string source for autocompletion at that point
; returns list of ranks in completion list
(define (test-file file-mod filename)
  (define percent .2)
  (define file-string (file->string filename))
  (define word-list (string->words file-string))
  (define total-words (length word-list))
  (define word-count (inexact->exact (ceiling (* (length word-list) percent))))
  (define words (list-tail (shuffle word-list) (- total-words word-count)))
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