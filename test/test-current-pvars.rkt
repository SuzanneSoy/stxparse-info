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