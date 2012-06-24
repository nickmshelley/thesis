#lang racket
(require "completers/all-tokens.rkt")

; gets all racket files in directory
; TODO: include subdirectories and only include .rkt or .ss files
(define (get-all-files dir)
  (for/list ([file (directory-list dir)])
    (build-path dir file)))

(define (test-all-files files)
  (map test-file files))

(define (string-w/o-word s w)
  (string-append (substring s 0 (word-pos w))
                 (substring s (+ (word-pos w) (string-length (word-str w))))))

(define (test-file file)
  (define text-string (file->string file))
  (define words (string->words text-string))
  (for/list ([word words])
    (define completions (get-completions (string-w/o-word text-string word) 0 ""))
    (define result (member (word-str word) completions))
    (if result
        (- (length completions) (length result))
        #f)))

(module+ main
  (test-all-files (get-all-files "test-files")))