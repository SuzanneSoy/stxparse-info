#lang racket
(require stxparse-info/parse
         stxparse-info/current-pvars
         racket/stxparam
         rackunit)

(define-syntax (list-pvars stx)
  #`'#,(current-pvars))

(check-equal? (list-pvars)
              '())

(check-equal? (syntax-parse #'(1 2 3 a b c)
                [(x y:nat ... {~parse w (list-pvars)} z ...)
                 (syntax->datum #`[w #,(list-pvars)])])
              '([y x] [z w y x]))

(check-equal? (list-pvars)
              '())

;; Check that the identifier has the right scopes
(define-syntax (ref-nth-pvar stx)
  (syntax-case stx ()
    [(_ n)
     (number? (syntax-e #'n))
     #`#'#,(let ([pvar (list-ref (current-pvars) (syntax-e #'n))])
             (datum->syntax pvar (syntax-e pvar) stx))]))

(check-equal? (syntax-parse #'1
                [x
                 (syntax->datum (ref-nth-pvar 0))])
              1)

(check-equal? (syntax-parse #'1
                [x
                 (cons (syntax->datum (ref-nth-pvar 0))
                       (syntax-parse #'2
                         [x
                          (list (syntax->datum (ref-nth-pvar 0))
                                (syntax->datum (ref-nth-pvar 1)))]))])
              '(1 2 1))