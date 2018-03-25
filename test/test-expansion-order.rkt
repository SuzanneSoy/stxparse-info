#lang racket

;; This is a quick experiment to see in what order are macros expanded.
;;
;; It is necessary to understand this in order to internally build a stack of
;; definitions of pattern variables, and correctly pop pvars from the stack when
;; they go out of scope.

;; Macros in definition contexts are expanded in a breadth-first order
;; Macros in expression contexts are expanded in a breadth-first order
;;
;; Within a scope (let or top-level), all definitions are expanded,
;; then all the expressions

(require rackunit)

(define-for-syntax order '())
(define-for-syntax (record-order x)
  (set! order (cons x order)))

(define-syntax (d stx)
  (syntax-case stx ()
    [(_ a)
     (begin (record-order `(d . ,(syntax-e #'a)))
            #'(define x 42))]))

(define-syntax (e stx)
  (syntax-case stx ()
    [(_ a)
     (begin (record-order `(e . ,(syntax-e #'a)))
            #'42)]))

(define (expr x) (void))

(d "+0 012")
(expr (e "?3 012"))

(let ()
  (d "+4 012_45")
  (expr (e "?6 012_45"))
  ;; here, we're evaluating an "e" in a definition context,
  ;; therefore it does not know that 5 will exist.
  (e "¿5 012_4X\"")
  ;; wrapping it with #%expression ensures that it runs after all definitions
  ;; in the current scope (of course it then cannot introduce new definitions).
  (#%expression (e "e6 012_46\"'"))
  (let ()
    (d "+7 012_45_7")
    (expr (e "?8 012_45_7"))
    (d "+7 012_45_7'")
    (expr (e "?8 012_45_7'")))
  (d "+5 012_45")
  (expr (e "?9 012_45")))

(d "+1 012")
(expr (e "?A 012"))

(let ()
  (d "+B 012_B")
  (expr (e "?C 012_B")))

(d "+2 012")
(expr (e "?D 012"))
(check-equal? (let-syntax ([get (λ (stx) #`'#,(reverse order))])
                get)
              '((d . "+0 012")
                (d . "+1 012")
                (d . "+2 012")
                (e . "?3 012")
                (d . "+4 012_45")
                (e . "¿5 012_4X\"")
                (d . "+5 012_45")
                (e . "?6 012_45")
                (e . "e6 012_46\"'")
                (d . "+7 012_45_7")
                (d . "+7 012_45_7'")
                (e . "?8 012_45_7")
                (e . "?8 012_45_7'")
                (e . "?9 012_45")
                (e . "?A 012")
                (d . "+B 012_B")
                (e . "?C 012_B")
                (e . "?D 012")))