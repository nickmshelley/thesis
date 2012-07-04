#lang racket
(require "completers/all-tokens.rkt")

; gets all racket files in directory
; TODO: include subdirectories and only include .rkt or .ss files
(define (get-all-files dir)
  (for/list ([file (directory-list dir)])
    (build-path dir file)))

(define (test-all-files files)
  (map analyze (map test-file files)))

(define (string-w/o-word s w)
  (string-append (substring s 0 (word-pos w))
                 (substring s (+ (word-pos w) (string-length (word-str w))))))

#;(define (list-random-ref l)
    (list-ref l (random (length l))))

(define (test-file file)
  (define text-string (file->string file))
  (define words (string->words text-string))
  (list (path->string file)
        (for/list ([word words])
          (define completions (get-completions (string-w/o-word text-string word) 0 ""))
          (define result (member (word-str word) completions))
          (if result
              (- (length completions) (length result))
              #f))))

(define (analyze results)
  (define hits-list (filter identity (second results)))
  (define threshold 5)
  (list (first results)
        (for/list ([i (in-range threshold)])
          (count (λ (num) (= num i)) hits-list))
        (list (count (λ (num) (>= num threshold)) hits-list)
              (count not results))))

(define (display-results results)
  (printf "Results for file: ~a~n" (first results))
  (printf "Ranks of correct completions:~n")
  (for ([num (second results)]
        [i (in-range (length (second results)))])
    (printf "~a: ~a~n" (add1 i) num))
  (printf ">~a: ~a~n" (length (second results)) (first (third results)))
  (printf "Not included: ~a~n~n" (second (third results))))

(module+ main
  (for-each display-results (test-all-files (get-all-files "test-files"))))