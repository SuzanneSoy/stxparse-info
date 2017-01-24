#lang racket

(require (for-syntax syntax/parse/experimental/template)
         (for-syntax racket/base)
         (for-meta 2 racket/base)
         (for-meta 2 stxparse-info/parse/experimental/steal-box))

(begin-for-syntax
  (eval #'(begin (define-syntax (e2 stx)
                   #`(begin
                       (module #,(cdr (syntax-e stx)) racket
                         (provide (for-syntax e4a e4b))
                         ;(require syntax/parse/experimental/template)
                         (define-for-syntax e4a #,template-metafunction?)
                         (define-for-syntax e4b #,template-metafunction-var)
                         (module* e5 racket/base
                           (require (for-template (submod "..")))
                           (provide e4a e4b)))))
                 (e2 . e3))
        (module->namespace 'syntax/parse/experimental/template))
  (define e5a (dynamic-require '(submod 'e3 e5) 'e4a))
  (define e5b (dynamic-require '(submod 'e3 e5) 'e4b))
  (provide (rename-out [e5a template-metafunction?]
                       [e5b template-metafunction-var])))

