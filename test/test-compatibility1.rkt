#lang racket
(require ;syntax/parse
         ;syntax/parse/experimental/template
         stxparse-info/parse
         stxparse-info/parse/experimental/template)
(provide mf original-template)
(define-template-metafunction (mf stx)
  #'ok-metafunction-official-1)
(define-syntax-rule (original-template t)
  (template t))
  