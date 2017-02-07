#lang at-exp racket/base

(provide ovl
         ovl*
         orig)

(require scribble/manual
         (for-syntax racket/base
                     racket/function
                     racket/struct
                     racket/vector
                     racket/syntax
                     syntax/stx
                     syntax/strip-context))

(begin
  ;; From the type-expander docs:
  (define-for-syntax (strip-loc e)
    (cond [(syntax? e) (datum->syntax e (strip-loc (syntax-e e)) #f)]
          [(pair? e) (cons (strip-loc (car e)) (strip-loc (cdr e)))]
          [(vector? e) (vector-map strip-loc e)]
          [(box? e) (box (strip-loc (unbox e)))]
          [(prefab-struct-key e)
           => (λ (k) (apply make-prefab-struct
                            k
                            (strip-loc (struct->list e))))]
          [else e]))

  (define-syntax (orig stx)
    (syntax-case stx ()
      [(_ mod name ...)
       (with-syntax ([(prefixed ...)
                      (stx-map (λ (id) (format-id id "orig:~a" id))
                               #'(name ...))]
                     [(orig-module) (generate-temporaries #'(mod))])
         #`(begin
             (module #,(datum->syntax #'mod (syntax-e #'orig-module)) .
               #,(strip-context
                  #'(racket/base
                     (require (for-label (only-meta-in 0 (only-in mod
                                                                  name ...))))
                     (require scribble/manual)
                     (define prefixed @racket[name]) ...
                     (provide prefixed ...))))
             (require #,(datum->syntax #'mod `',(syntax-e #'orig-module)))))]))

  (define-syntax (ovl* stx)
    (syntax-case stx ()
      [(_ mod name ...)
       (with-syntax ([(prefixed ...)
                      (stx-map (λ (id) (format-id #'mod "orig:~a" id))
                               #'(name ...))]
                     [(stripped-name ...)
                      (stx-map strip-loc
                               #'(name ...))])
         #'(list
            @defidform[stripped-name]{
        Overloaded version of @|prefixed| from
        @racketmodname[mod].}
            ...))]))

  (define-syntax (ovl stx)
    (syntax-case stx ()
      [(self mod name ...)
       (identifier? #'mod)
       #'(self #:wrapper list mod name ...)]
      [(self #:wrapper wrapper mod name ...)
       (identifier? #'mod)
       #'(self #:wrapper wrapper #:require mod mod name ...)]
      [(_ #:wrapper wrapper #:require req mod name ...)
       #'(begin
           (orig req name ...)
           (wrapper (ovl* mod name ...)))])))