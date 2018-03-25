#lang racket

;; This is a quick experiment to check that a set of identifiers (syntax
;; transformers) can be maintained using a stack, while ensuring that when
;; the set is queried at compile-time, only those identifiers which are within
;; scope are returned.
;;
;; It is necessary to understand this in order to internally build a stack of
;; definitions of pattern variables, and correctly pop pvars from the stack when
;; they go out of scope.

(require rackunit)

(define-for-syntax order '())
(define-for-syntax (record-order x)
  (set! order (cons x order)))


(define-for-syntax stack '())
(define-for-syntax (push! e)
  (set! stack (cons e stack)))
(define-for-syntax (peek)
  (car stack))
(define-for-syntax (pop!)
  (set! stack (cdr stack)))
(define-for-syntax (pop…!)
  (when (not (null? stack))
    (unless (syntax-local-value (car stack) (λ () #f))
      ;(displayln (syntax->datum #`(pop #,(car stack))))
      (pop!)
      (pop…!))))

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ var)
     (begin
       (pop…!)
       (push! #'var)
       #'(define-syntax var 42))]))

(define-syntax (query stx)
  (syntax-case stx ()
    [(_ msg)
     (begin
       (pop…!)
       (record-order (syntax->datum #`(msg . #,stack)))
       #'(void))]))

(define (expr x) (void))


(define-syntax (macro stx)
  #'(def v2))

(def v1)
(macro)
(let ()
  (def v6.1)
  (query q6.2)
  (expr (query q6.4))
  (let ()
    (def v6.5.1)
    (void))
  (let ()
    (def v6.6.1)
    ;; These queries must *not* contain v6.5.1.
    (query q6.6.2)
    (expr (query q6.6.3))
    (void))
  (let ()
    (def v6.7.1)
    ;; These queries must *not* contain v6.5.1 nor v6.6.1.
    (query q6.7.2)
    (expr (query q6.7.3))
    (void))
  (def v6.3)
  (void))
(query q3)
(expr (query q7))
(def v4)
(query q5)
(expr (query q8))

(check-equal? (let-syntax ([get (λ (stx) #`'#,(reverse order))])
                get)
              '((q3 v2 v1)
                (q5 v4 v2 v1)
                (q6.2 v6.1 v4 v2 v1)
                (q6.4 v6.3 v6.1 v4 v2 v1)
                (q6.6.2 v6.6.1 v6.3 v6.1 v4 v2 v1)
                (q6.6.3 v6.6.1 v6.3 v6.1 v4 v2 v1)
                (q6.7.2 v6.7.1 v6.3 v6.1 v4 v2 v1)
                (q6.7.3 v6.7.1 v6.3 v6.1 v4 v2 v1)
                (q7 v4 v2 v1)
                (q8 v4 v2 v1)))