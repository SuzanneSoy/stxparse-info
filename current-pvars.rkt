#lang racket/base
(require racket/stxparam
         (for-syntax racket/base
                     racket/contract))

(provide (for-syntax (rename-out [get-current-pvars current-pvars]))
         with-pvars)

(define-syntax-parameter current-pvars '())

(define-syntax (with-pvars stx)
  (syntax-case stx ()
    [(_ (pvar ...) . body)
     (andmap identifier? (syntax->list #'(pvar ...)))
     (with-syntax ([(reverse-pvar ...) (reverse (syntax->list #'(pvar ...)))])
       #'(syntax-parameterize
             ([current-pvars (list* (quote-syntax reverse-pvar) ...
                                    (syntax-parameter-value #'current-pvars))])
           . body))]))

(begin-for-syntax
  (define/contract (get-current-pvars)
    (-> (listof identifier?))
    (syntax-parameter-value #'current-pvars)))