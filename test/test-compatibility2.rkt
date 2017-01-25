#lang racket
(require stxparse-info/parse
         stxparse-info/parse/experimental/template
         rackunit
         "test-compatibility1.rkt")

;; TODO: re-enable this, and do the test the other way round too
;;       (the official syntax/parse to from stxparse-info)
#;(check-equal? (syntax-parse #'(1 (2 3))
                  [(x {~optional y} ({~optional z} t))
                   (list (syntax->datum
                          (original-template (x (?? y no-y) (?? z no-z) t (mf))))
                         (syntax->datum
                          (template (x (?? y no-y) (?? z no-z) t (mf)))))])
                '((1 no-y 2 3 ok-metafunction-official-1)
                  (1 no-y 2 3 ok-metafunction-official-1)))
