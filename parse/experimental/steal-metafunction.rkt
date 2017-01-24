#lang racket

;; Manages to grasp the template-metafunction via (namespace-mapped-symbols)
;; within the eval.
(module extracted-template-metafunction racket/base
  (require (for-syntax syntax/parse/experimental/template)
           (for-syntax racket/base)
           (for-meta 2 racket/base)
           (for-meta 2 stxparse-info/parse/experimental/steal-box))
  (define-syntax (fu stx)
    (syntax-case stx ()
      [(_ id-mf? id-mf-v)
       (let ()
         (eval #'(begin
                   (require (for-syntax
                             stxparse-info/parse/experimental/steal-box))
                   (define-template-metafunction (mf stx)
                     #'1)
                   (define-syntax (extract stx)
                     ;; Use 3D syntax to return the value:
                     (displayln (namespace-mapped-symbols))
                     ;(displayln (eval 'template-metafunction))
                     (define ctor (namespace-variable-value
                                   (for/first ([sym (namespace-mapped-symbols)]
                                               #:when (regexp-match #rx"template-metafunction[0-9].*" (symbol->string sym)))
                                     sym)))
                     (displayln (list ctor (ctor 5165163)))
                     (displayln (template-metafunction? (syntax-local-value #'mf)))
                     (set-box! bx template-metafunction?)
                     #'(void)
                     #;#`(values #,template-metafunction?
                                 #,template-metafunction-var))
                   (extract))
               (module->namespace 'syntax/parse/experimental/template))
         #`(begin
             (define-for-syntax id-mf? 0)
             (define-for-syntax id-mf-v 1)))]))
  (fu out-id-mf? out-id-mf-v)
  (begin-for-syntax
    (define-for-syntax rsl bx))
  
  #;(begin-for-syntax
      (define-syntax (br stx)
        (displayln rsl)
        #'(void))
      (br))
  #;(begin-for-syntax
      (begin-for-syntax
        (displayln rsl)))
  (provide (for-meta 2 rsl #;out-id-mf? #;out-id-mf-v)))

#;(require (rename-in (for-template 'extracted-template-metafunction)
                      [out-id-mf? template-metafunction?]
                      [out-id-mf-v template-metafunction-var]))
(require (for-meta -2 'extracted-template-metafunction))
(displayln rsl)

#;(provide template-metafunction?
           template-metafunction-var)
