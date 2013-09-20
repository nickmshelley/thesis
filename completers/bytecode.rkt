#lang racket/base

(require compiler/zo-parse
         racket/function
         racket/list
         racket/system
         racket/string)

(provide get-zo-completions)

;; file is a path-string to the file
(define (get-zo-completions filename text prefix pos)
  (system (format "/Applications/plt/racket/bin/raco make ~a" filename))
  (define-values (base name _) (split-path filename))
  (define compiled-file (build-path base
                                    "compiled"
                                    (string-append (string-replace (path->string name) "." "_")
                                                   ".zo")))
  (define res (with-input-from-file compiled-file zo-parse))
  (append (require-tokens res base)
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

;;(define file "/Users/heather/Nick/thesis/thesis/util.rkt")
;;(get-zo-completions file "" "" 0)