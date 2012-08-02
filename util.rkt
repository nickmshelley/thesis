#lang racket/base
(require "completers/all-tokens.rkt"
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
  (check-equal? (string-truncated-from-word test-string (third words))
                "(this is) ")
  (check-equal? (string-truncated-from-word test-string (first words))
                "(")
  (check-equal? (string-truncated-from-word test-string (fourth words))
                "(this is) a \""))

(define (list-random-ref l)
  (list-ref l (random (length l))))

(define (percent-of-words-from-file percent file-string)
  (define percent 1)
  (define word-list (file-string->word-symbols file-string))
  (define total-words (length word-list))
  (define word-count (inexact->exact (ceiling (* (length word-list) percent))))
  (define words (list-tail (shuffle word-list) (- total-words word-count)))
  words)

; file-string->word-symbols : string -> list-of-word
(define (file-string->word-symbols s)
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
  (require rackunit)
  (define (word-equal? w1 w2)
    (and (equal? (word-pos w1) (word-pos w2))
         (equal? (word-str w1) (word-str w2))))
  (define str "#lang racket (define \"hi there\" 5 8 + < be) (hi there)")
  (define results (file-string->word-symbols str))
  (check-equal? (length results) 6)
  (check-true (word-equal? (first results) (word "define" 14)))
  (check-true (word-equal? (second results) (word "+" 36))
              (format "actual: ~a" (second results)))
  (check-true (word-equal? (third results) (word "<" 38)))
  (check-true (word-equal? (fourth results) (word "be" 40)))
  (check-true (word-equal? (fifth results) (word "hi" 45)))
  (check-true (word-equal? (sixth results) (word "there" 48))))