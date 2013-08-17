#lang racket

(provide get-macro-completions)

(define (get-macro-completions text prefix pos)
  (sort
   (map 
    symbol->string
    (append
     (parameterize ([current-namespace (make-base-empty-namespace)])
       (define lang (get-lang text))
       (when lang (namespace-require (quasiquote ,lang)))
       (namespace-mapped-symbols))
     (parameterize ([current-namespace (make-base-empty-namespace)])
       (define requires (get-requires text))
       (map namespace-require requires)
       (namespace-mapped-symbols))
     (parameterize ([current-namespace (make-base-empty-namespace)])
       (define structs (get-structs text))
       (cond
         [(not (empty? structs))
          (namespace-require 'racket/base)
          (define base (namespace-mapped-symbols))
          (for-each (compose eval read open-input-string) structs)
          (filter-not (λ (v)
                        (member v base))
                      (namespace-mapped-symbols))]
         [else empty]))))
   string<=?))
(module+ test
  (require rackunit)
  (check-equal? (get-macro-completions "" "" 0)
                empty)
  (check-equal? (get-macro-completions "(require racket/list \"all-tokens.rkt\") (struct thing (x y)) (struct another (z a))" "" 0)
                (map symbol->string '(add-between another another-a another-z another3.2 another? append* append-map argmax argmin cons? count drop drop-right dropf dropf-right eighth empty empty? fifth filter-map filter-not first flatten fourth get-completions get-completions/keywords-and-position get-completions/nest in-permutations last last-pair make-list ninth partition permutations range remove-duplicates rest second seventh shuffle sixth split-at split-at-right splitf-at splitf-at-right struct:another struct:thing take take-right takef takef-right tenth thing thing-x thing-y thing1.1 thing? third))))

(define (get-lang text-string)
  (define match (regexp-match #px"^#lang\\s+(\\S+)" text-string))
  (and match (string->symbol (second match))))
(module+ test
  (check-equal? (get-lang "hi") #f)
  (check-equal? (get-lang "#lang racket") 'racket)
  (check-equal? (get-lang "#lang racket/base") 'racket/base))

(define (get-requires text-string)
  (define matches (regexp-match* #px"\\(require\\s+([^\\(\\)]+)\\)" text-string #:match-select second))
  (map (λ (arg)
         (if (char=? (string-ref arg 0) #\")
             (string-trim arg "\"")
             (string->symbol arg)))
       (filter
        (λ (str)
          (> (string-length str) 0))
        
        (flatten 
         (for/list ([match matches])
           (string-split match #px"\\s"))))))
(module+ test
  (check-equal? (get-requires "hi") empty)
  (check-equal? (get-requires "(require a)") '(a))
  (check-equal? (get-requires "#lang racket (require a)") '(a))
  (check-equal? (get-requires "(require a \"b\" c)") '(a "b" c))
  (check-equal? (get-requires "(require \"a\" b) (require c \"d\")") '("a" b c "d")))

(define (get-structs text-string)
  (regexp-match* #px"\\((?:define\\-)?struct[^\\(\\)]+\\([^\\(\\)]+\\)[^\\(\\)]*\\)" text-string))
(module+ test
  (check-equal? (get-structs "hi") empty)
  (check-equal? (get-structs "(struct posn (x y) #:transparent)")
                '("(struct posn (x y) #:transparent)"))
  (check-equal? (get-structs "(define-struct color-posn posn (hue))")
                '("(define-struct color-posn posn (hue))"))
  (check-equal? (get-structs "#lang racket (struct posn (x y) #:transparent) (define a 3) a (define-struct color-posn posn (hue)) (print a)")
                '("(struct posn (x y) #:transparent)"
                  "(define-struct color-posn posn (hue))")))


