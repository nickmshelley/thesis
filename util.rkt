#lang racket/base
(require "word.rkt"
         racket/list
         srfi/13)

(provide get-all-source-files
         string-w/o-word
         string-truncated-from-word
         list-random-ref
         percent-of-words-from-file)

; get-all-source-files : path-string -> list-of-filepath
; gets all racket files in directory
(define (get-all-source-files dir)
  (for/list ([path (in-directory dir)]
             #:when (and (file-exists? path)
                         (or (string-suffix? ".rkt" (path->string path))
                             (string-suffix? ".ss" (path->string path)))))
    path))

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
  (set! test-string "(this is) a \"test\" you know?")
  (set! words (string->words test-string))
  (check-equal? (string-truncated-from-word test-string (third words))
                "(this is) ")
  (check-equal? (string-truncated-from-word test-string (first words))
                "(")
  (check-equal? (string-truncated-from-word test-string (fourth words))
                "(this is) a \"")
  (check-equal? (string-truncated-from-word test-string (sixth words))
                "(this is) a \"test\" you "))

(define (list-random-ref l)
  (list-ref l (random (length l))))

; percent-of-words-from-file : float[0, 1] string -> list-of-word
(define (percent-of-words-from-file percent file-string)
  (define word-list (string->word-symbols file-string))
  (define total-words (length word-list))
  (define word-count (inexact->exact (ceiling (* (length word-list) percent))))
  (define words (list-tail (shuffle word-list) (- total-words word-count)))
  words)
(module+ test
  (define percent-str "#lang racket (define \"hi there\" 5 8 + < be) (hi there)")
  (check-equal? (length (percent-of-words-from-file 1 percent-str))
                6)
  (check-equal? (length (percent-of-words-from-file .5 percent-str))
                3))

; file-string->word-symbols : string -> list-of-word
; the main purpose of this method is to ignore strings in the code
; this prevents the output from being tainted by non-code symbols
(define (string->word-symbols s)
  (define code-stx
    (parameterize ([read-accept-reader #t]
                   [read-accept-lang #t])
      (read-syntax "name" (open-input-string s))))
  (define stx 
    (if (string-prefix? "#lang racket" s)
        (rest (syntax-e (fourth (syntax-e code-stx))))
        (list code-stx)))
  (let loop ([stx-lst stx])
    (if (empty? stx-lst)
        empty
        (let ([fst (syntax-e (first stx-lst))])
          (cond
            [(symbol? fst)
             (cons (word (symbol->string fst) 
                         (sub1 (syntax-position (first stx-lst))))
                   (loop (rest stx-lst)))]
            [(syntax? fst)
             (loop (cons fst (rest stx-lst)))]
            [(list? fst)
             (loop (append fst (rest stx-lst)))]
            [else
             (loop (rest stx-lst))])))))
(module+ test
  (define str "#lang racket (define \"hi there\" 5 8 + < be) (hi there)")
  (define results (string->word-symbols str))
  (check-equal? (length results) 6)
  (check-true (word-equal? (first results) (word "define" 14)))
  (check-true (word-equal? (second results) (word "+" 36)))
  (check-true (word-equal? (third results) (word "<" 38)))
  (check-true (word-equal? (fourth results) (word "be" 40)))
  (check-true (word-equal? (fifth results) (word "hi" 45)))
  (check-true (word-equal? (sixth results) (word "there" 48))))