#lang racket
(require plot)

(define GRAPH-HEIGHT 400)
(define GRAPH-WIDTH 800)

(struct rankings (filename ranked unranked missed) #:transparent)

(module+ main
  (parameterize ([plot-font-size 17])
    (produce-ranker-graphs)
    (produce-checker-graphs)))

(define (produce-checker-graphs)
  (define remove-results
    (list (list 'proximity 2294 11789)
          (list 'nest 2399 11263)
          (list 'keywords 2685 11882)
          (list 'macros 1701 16867)
          (list 'bytecode 1425 15301)
          (list 'combined 2496 11131)))
  (display-checker remove-results "Remove")
  (display-checker-percent (percent-results remove-results) "Remove-Percent")
  (define truncate-results
    (list (list 'proximity 2228 11758)
          (list 'nest 2309 11573)
          (list 'keywords 2947 11290)
          (list 'macros 1667 16840)
          (list 'bytecode 1487 15109)
          (list 'combined 2246 11904)))
  (display-checker truncate-results "Truncate")
  (display-checker-percent (percent-results truncate-results) "Truncate-Percent"))

(define (percent-results results)
  (for/list ([l (in-list results)])
    (list (first l)
          (* 100
             (/ (second l)
                (+ (second l) (third l)))))))

(define (produce-ranker-graphs)
  (define remove-results
    (list (cons 'proximity (rankings "All" '(88 131 244 392 284) 3448 404))
          (cons 'nest (rankings "All" '(144 593 463 482 325) 2566 418))
          (cons 'keywords (rankings "All" '(48 79 190 260 249) 3767 398))
          (cons 'macros (rankings "All" '(0 0 0 0 0) 2283 2706))
          (cons 'bytecode (rankings "All" '(1 0 0 0 10) 2823 2157))
          (cons 'combined (rankings "All" '(133 586 478 454 295) 2943 102))))
  (display-rankings remove-results "Remove")
  (display-combined remove-results "Remove-combined")
  (display-uber-combined remove-results "Remove-uber-combined")
  (define truncate-results
    (list (cons 'proximity (rankings "All" '(83 282 272 279 206) 2837 1032))
          (cons 'nest (rankings "All" '(182 576 401 303 267) 2273 989))
          (cons 'keywords (rankings "All" '(68 271 241 261 216) 2965 969))
          (cons 'macros (rankings "All" '(0 0 0 0 0) 2288 2701))
          (cons 'bytecode (rankings "All" '(1 0 1 0 9) 2838 2142))
          (cons 'combined (rankings "All" '(167 623 337 331 311) 2906 316))))
  (display-rankings truncate-results "Truncate")
  (display-combined truncate-results "Truncate-combined")
  (display-uber-combined truncate-results "Truncate-uber-combined"))

(define (display-checker results name)
  (define skip (+ 3 (length results)))
  (plot-file
   (for/list ([res (in-list results)]
              [i (in-range (length results))])
     (define label (symbol->string (first res)))
     (define passed (second res))
     (define failed (third res))
     (define color (add1 i))
     (discrete-histogram
      (append (list (vector 'Passed passed))
              (list (vector 'Failed failed)))
      #:skip skip #:x-min i #:y-min .1 #:label label #:color color #:line-color color))
   (format "/Users/heather/Nick/thesis/thesis/output/synthesis/checker/~a.png" name)
   #:title " "
   #:x-label " "
   #:y-label "Amount"
   #:height GRAPH-HEIGHT
   #:width GRAPH-WIDTH))

(define (display-checker-percent results name)
  (plot-file
   (for/list ([res (in-list results)]
              [i (in-range (length results))])
     (define label (symbol->string (first res)))
     (define percent (second res))
     (define color (add1 i))
     (discrete-histogram
      (append (list (vector (string->symbol "Method") percent)))
      #:x-min i #:label label #:color color #:line-color color))
   (format "/Users/heather/Nick/thesis/thesis/output/synthesis/checker/~a.png" name)
   #:title " "
   #:x-label " "
   #:y-label "Percent Passed"
   #:height GRAPH-HEIGHT
   #:width GRAPH-WIDTH))

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
   #:title " "
   #:x-label " "
   #:y-label "Amount"
   #:height GRAPH-HEIGHT
   #:width GRAPH-WIDTH))

(define (display-combined results name)
  (define skip (+ 3 (length results)))
  (plot-file
   (for/list ([res (in-list results)]
              [i (in-range (length results))])
     (define label (symbol->string (car res)))
     (define current-rankings (cdr res))
     (define color (add1 i))
     (discrete-histogram
      (list (vector 'Ranked (apply + (rankings-ranked current-rankings)))
            (vector 'Unranked (rankings-unranked current-rankings))
            (vector 'Missed (rankings-missed current-rankings)))
      #:skip skip #:x-min i #:y-min .1 #:label label #:color color #:line-color color))
   (format "/Users/heather/Nick/thesis/thesis/output/synthesis/ranker/~a.png" name)
   #:title " "
   #:x-label " "
   #:y-label "Amount"
   #:height GRAPH-HEIGHT
   #:width GRAPH-WIDTH))

(define (display-uber-combined results name)
  (define skip (+ 3 (length results)))
  (plot-file
   (for/list ([res (in-list results)]
              [i (in-range (length results))])
     (define label (symbol->string (car res)))
     (define current-rankings (cdr res))
     (define color (add1 i))
     (discrete-histogram
      (list (vector 'Hit (+ (apply + (rankings-ranked current-rankings))
                            (rankings-unranked current-rankings)))
            (vector 'Missed (rankings-missed current-rankings)))
      #:skip skip #:x-min i #:y-min .1 #:label label #:color color #:line-color color))
   (format "/Users/heather/Nick/thesis/thesis/output/synthesis/ranker/~a.png" name)
   #:title " "
   #:x-label " "
   #:y-label "Amount"
   #:height GRAPH-HEIGHT
   #:width GRAPH-WIDTH))

(define (display-checked results name)
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
   #:title " "
   #:x-label " "
   #:y-label "Amount"
   #:height GRAPH-HEIGHT
   #:width GRAPH-WIDTH))

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