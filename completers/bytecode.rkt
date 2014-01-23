#lang racket/base

(require compiler/zo-parse
         compiler/cm
         racket/function
         racket/dict
         racket/list)

(provide get-zo-completions)

;; file is a path-string to the file
(define (get-zo-completions filename text prefix pos)
  (with-handlers ([(λ (x) #t) (λ (e) (printf "Ignoring exception ~a~n~n" e) empty)])
    (managed-compile-zo filename)
    (define-values (base name _) (split-path filename))
    (define compiled-file (build-path base
                                      "compiled"
                                      (path-add-suffix name ".zo")))
    (define res (with-input-from-file compiled-file zo-parse))
    (append (require-tokens res base)
            (top-levels res))))

(define (require-tokens res base)
  (define requires 
    (map (curry mpi-submod base) 
         (dict-ref (mod-requires (compilation-top-code res)) 0)))
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (map namespace-require requires)
    (sorted-strings-from-symbols (namespace-mapped-symbols))))

(define (mpi-submod base mpi)
  (define-values (submod mp) (module-path-index-split mpi))
  (if (symbol? submod)
      submod
      (build-path base submod)))

(define (top-levels res)
  (sorted-strings-from-symbols
   (filter symbol? (prefix-toplevels (mod-prefix (compilation-top-code res))))))

(define (sorted-strings-from-symbols symbols)
  (sort (map symbol->string symbols) string<=?))

;;(define file "/Users/heather/Nick/thesis/thesis/util.rkt")
;;(get-zo-completions file "" "" 0)