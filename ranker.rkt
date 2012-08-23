#lang racket/base
(require "completers/all-tokens.rkt"
         "word.rkt"
         "util.rkt"
         racket/list
         racket/file
         racket/function
         plot)

; A rankings combines a filename with analyzed stats
; Filename : string
; ranked : list of number - index is rank and number is how many of that rank
; unranked : number
; missed : number
(struct rankings (filename ranked unranked missed))
(module+ test
  (require rackunit)
  ;rankings-equal? : rankings rankings -> boolean
  (define (rankings-equal? r1 r2)
    (if (and (equal? (rankings-filename r1) (rankings-filename r2))
             (equal? (rankings-ranked r1) (rankings-ranked r2))
             (equal? (rankings-unranked r1) (rankings-unranked r2))
             (equal? (rankings-missed r1) (rankings-missed r2)))
        #t
        (begin (printf "r1: (~a ~a ~a ~a)~nr2: (~a ~a ~a ~a)~n"
                       (rankings-filename r1)
                       (rankings-ranked r1)
                       (rankings-unranked r1)
                       (rankings-missed r1)
                       (rankings-filename r2)
                       (rankings-ranked r2)
                       (rankings-unranked r2)
                       (rankings-missed r2))
               #f))))

; test-all-files : list-of-filepath (string word -> string) -> list-of-rankings
(define (test-all-files files file-mod)
  (define percent 1)
  (map (λ (filename)
         (test-file file-mod
                    filename
                    (file->string filename)
                    percent))
       files))

; test-file : (string word -> string) string -> rankings
; file-mod modifies the string source for autocompletion at that point
; returns list of ranks in completion list
(define (test-file file-mod filename file-string percent)
  (define words (percent-of-words-from-file percent file-string))
  (define results 
    (for/list ([word words])
      (define completions (get-completions (file-mod file-string word) ""))
      (define result (member (word-str word) completions))
      (and result
           (- (length completions) (length result)))))
  (define-values (ranked unranked missed) (analyze results))
  (rankings filename ranked unranked missed))
(module+ test
  ;all completions: (+ 5 a be c define hi lang racket there you)
  (define str "#lang racket be (define you \"hi there you\" 5 + be) (define + there you) a c")
  (define rank1 (test-file string-w/o-word "name" str 1))
  (define-values (ranked unranked missed)
    ;these results can be confusing because only symbols as defined by string->word-symbols are tested
    ;but completions gets all words as defined by string->words
    (analyze (list 3 5 10 0 3 5 0 9 10 #f #f)))
  (check-true (rankings-equal? rank1 (rankings "name" ranked unranked missed)))
  (define rank2 (test-file string-truncated-from-word "name" str 1))
  (set!-values (ranked unranked missed)
    ;remember that the positions change with every word (since later words like a and c get truncated)
    (analyze (list #f #f #f #f 2 3 0 7 8 #f #f)))
  (check-true (rankings-equal? rank2 (rankings "name" ranked unranked missed))))

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

(define (display-rankings alter-method ranks)
  #;(plot-new-window? #t)
  (define filename (rankings-filename ranks))
  (define ranked (rankings-ranked ranks))
  (define unranked (rankings-unranked ranks))
  (define missed (rankings-missed ranks))
  (display (plot (discrete-histogram
                  (append (ranked->vectors ranked)
                          (list (vector 'Unranked unranked))
                          (list (vector 'Missed missed))))
                 #:title (format "~a: ~a" filename alter-method)
                 #:x-label "Rank"
                 #:y-label "Amount")))

; ranked->vectors : list-of-number -> list-of #(number number)
(define (ranked->vectors ranked)
  (for/list ([i (in-range 1 (add1 (length ranked)))]
             [num ranked])
    (vector i num)))

(module+ main
  (for-each (curry display-rankings "Remove")
            (test-all-files 
             (get-all-source-files "test-files") 
             string-w/o-word))
  (for-each (curry display-rankings "Truncate")
            (test-all-files 
             (get-all-source-files "test-files") 
             string-truncated-from-word)))