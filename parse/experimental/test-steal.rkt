#lang racket
(require syntax/parse/experimental/template
         stxparse-info/parse/experimental/steal-metafunction2)
(define-template-metafunction (mf stx)
  #'1)
(provide mf)

(let ()
  (define-syntax (foo stx)
    (displayln
     (template-metafunction?
      (syntax-local-value #'mf)))
    #''ok)
  (foo))