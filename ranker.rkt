#lang racket
(require "completers/all-tokens.rkt")

; gets all racket files in directory
; TODO: include subdirectories and only include .rkt or .ss files
(define (get-all-files dir)
  (for/list ([file (directory-list dir)])
    (build-path dir file)))

(define (test-all-files files file-mod)
  (map analyze (map test-file files (make-list (length files) file-mod))))

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

#;(define (list-random-ref l)
    (list-ref l (random (length l))))

(define (test-file file file-mod)
  (define text-string (file->string file))
  (define words (string->words text-string))
  (list (path->string file)
        (for/list ([word words])
          (define completions (get-completions (file-mod text-string word) 0 ""))
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
              (count not (second results)))))
(module+ test
  (check-equal? (analyze (list "name" (list 6 6 4 4 2 1 0 0 5 8 9 #f #f)))
                (list "name" (list 2 1 1 0 2) (list 5 2))))

(define (display-results results)
  (printf "Results for file: ~a~n" (first results))
  (printf "Ranks of correct completions:~n")
  (for ([num (second results)]
        [i (in-range (length (second results)))])
    (printf "~a: ~a~n" (add1 i) num))
  (printf ">~a: ~a~n" (length (second results)) (first (third results)))
  (printf "Not included: ~a~n~n" (second (third results))))

(module+ main
  (printf "Alter file method: Remove~n~n")
  (for-each display-results (test-all-files (get-all-files "test-files") string-w/o-word))
  (printf "Alter file method: Truncate~n~n")
  (for-each display-results (test-all-files (get-all-files "test-files") string-truncated-from-word)))