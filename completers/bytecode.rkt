#lang racket/base

(require compiler/zo-parse
         racket/function
         racket/list)

;; file is a path-string to the compiled zo file
(define (get-zo-completions file)
  (define comps (explode-path file))
  (define-values (base-comps others) (split-at comps (- (length comps) 2)))
  (define base (apply build-path base-comps))
  (define res (with-input-from-file file zo-parse))
  res
  #;(append (require-tokens res base)
          (top-levels res)))

(define (require-tokens res base)
  (define requires (map (curry mpi-submod base) (cdr (assoc 0 (mod-requires (compilation-top-code res))))))
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (map namespace-require requires)
    (sort (map symbol->string (namespace-mapped-symbols)) string<=?)))

(define (mpi-submod base mpi)
  (define-values (submod mp) (module-path-index-split mpi))
  (if (symbol? submod)
      submod
      (build-path base submod)))

(define (top-levels res)
  (sort (map symbol->string
             (filter symbol? (prefix-toplevels (mod-prefix (compilation-top-code res)))))
        string<=?))

(define file "/Users/heather/Nick/thesis/thesis/compiled/util_rkt.zo")
(get-zo-completions file)