#lang racket
(require plot)

(struct rankings (filename ranked unranked missed) #:transparent)

(module+ main
  (define remove-results
    (list (cons 'naive (rankings "All" '(88 131 244 392 284) 3448 404))
          (cons 'nest (rankings "All" '(144 593 463 482 325) 2566 418))
          (cons 'keywords (rankings "All" '(56 80 153 286 196) 3817 403))
          (cons 'macros (rankings "All" '(0 0 0 0 0) 2283 2706))
          (cons 'bytecode (rankings "All" '(1 0 0 0 10) 2823 2157))
          (cons 'combined (rankings "All" '(133 586 478 454 295) 2943 102))))
  (display-rankings remove-results "Remove")
  (define truncate-results
    (list (cons 'naive (rankings "All" '(83 282 272 279 206) 2837 1032))
          (cons 'nest (rankings "All" '(182 576 401 303 267) 2273 989))
          (cons 'keywords (rankings "All" '(48 250 268 238 189) 2990 1008))
          (cons 'macros (rankings "All" '(0 0 0 0 0) 2288 2701))
          (cons 'bytecode (rankings "All" '(1 0 1 0 9) 2838 2142))
          (cons 'combined (rankings "All" '(167 623 337 331 311) 2906 316))))
  (display-rankings truncate-results "Truncate"))

(define (display-rankings results name)
  (define skip (+ 3 (length results)))
  (plot-file
   (for/list ([res (in-list results)]
              [i (in-range (length results))])
     (define label (symbol->string (car res)))
     (define current-rankings (cdr res))
     (define color (add1 i))
     (discrete-histogram
      (append (ranked->vectors (rankings-ranked current-rankings))
              (list (vector 'Unranked (rankings-unranked current-rankings)))
              (list (vector 'Missed (rankings-missed current-rankings))))
      #:skip skip #:x-min i #:y-min .1 #:label label #:color color #:line-color color))
   (format "/Users/heather/Nick/thesis/thesis/output/synthesis/ranker/~a.png" name)
   #:title name
   #:x-label "Rank"
   #:y-label "Amount"
   #:height 800
   #:width 1800))

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