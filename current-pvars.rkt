(module current-pvars '#%kernel
  (#%provide (for-syntax current-pvars
                         current-pvars+unique)
             with-pvars
             define-pvars)
  
  (#%require racket/private/small-scheme
             (for-syntax '#%kernel
                         racket/private/qq-and-or
                         racket/private/stx))
  (begin-for-syntax
    (define-values (current-pvars-param-guard)
      (lambda (x)
        ;; TODO: add condition: elements should be pairs of identifiers?
        ;; Skip the guard, otherwise each operation is O(n). TODO: use a
        ;; push/pop API which does the check on the head of the list instead.
        #;(if (list? x)
              x
              (error "current-pvars-param should be a list"))
        x))
    
    (define-values (current-pvars-param)
      (make-parameter '() current-pvars-param-guard))

    (define-values (current-pvars)
      (lambda ()
        (pop-unreachable-pvars)
        (map car (current-pvars-param))))
    
    (define-values (current-pvars+unique)
      (lambda ()
        (pop-unreachable-pvars)
        (current-pvars-param)))

    (define-values (syntax*->list)
      (λ (stxlist)
        (syntax->list (datum->syntax #f stxlist))))

    (define-values (pop-unreachable-pvars)
      (lambda ()
        (if (or (null? (current-pvars-param))
                (syntax-local-value (caar (current-pvars-param))
                                    (λ () #f)))
            (void)
            (begin
              (current-pvars-param (cdr (current-pvars-param)))
              (pop-unreachable-pvars))))))

  ;; (with-pvars [pvar ...] . body)
  (define-syntaxes (with-pvars)
    (lambda (stx)
      (if (not (and (stx-pair? stx)
                    (identifier? (stx-car stx))
                    (stx-pair? (stx-cdr stx))
                    (syntax*->list (stx-car (stx-cdr stx)))
                    (andmap identifier?
                            (syntax*->list (stx-car (stx-cdr stx))))))
          (raise-syntax-error 'with-pvars "bad syntax" stx)
          (void))
      (let* ([pvars (syntax*->list (stx-car (stx-cdr stx)))]
             [body (stx-cdr (stx-cdr stx))])
        (datum->syntax
         (quote-syntax here)
         `(let-values ()
            (define-pvars ,@pvars)
            ,@body))))
    #;(lambda (stx)
        (if (not (and (stx-pair? stx)
                      (identifier? (stx-car stx))
                      (stx-pair? (stx-cdr stx))
                      (syntax*->list (stx-car (stx-cdr stx)))
                      (andmap identifier?
                              (syntax*->list (stx-car (stx-cdr stx))))))
            (raise-syntax-error 'with-pvars "bad syntax" stx)
            (void))
        (let* ([pvars (reverse (syntax*->list (stx-car (stx-cdr stx))))]
               [unique-at-runtime (map gensym (map syntax-e pvars))]
               [pvars+unique (map cons pvars unique-at-runtime)]
               [body (stx-cdr (stx-cdr stx))]
               [do-unique-at-runtime (map (λ (id pvar)
                                            `[(,id) (gensym (quote ,pvar))])
                                          unique-at-runtime
                                          pvars)]
               [wrapped-body (datum->syntax
                              (quote-syntax here)
                              `(let-values (,@do-unique-at-runtime)
                                 ,@body))])

          (pop-unreachable-pvars)

          (with-continuation-mark
              parameterization-key
            (extend-parameterization
             (continuation-mark-set-first #f parameterization-key)
             current-pvars-param
             (append pvars+unique
                     (current-pvars-param)))
            (let-values ([(stx opaque)
                          (syntax-local-expand-expression wrapped-body #t)])
              opaque))
        
          ;; above is the manual expansion of:
          #;(parameterize ([current-pvars-param
                            (list* stxquoted-pvars+unique
                                   (current-pvars-param))])
              … syntax-local-expand-expression …))))

  ;; (define-pvars pv1 … pvn)
  (define-syntaxes (define-pvars)
    (lambda (stx)
      (if (not (and (stx-pair? stx)
                    (identifier? (stx-car stx))
                    (syntax*->list (stx-cdr stx))
                    (andmap identifier?
                            (syntax*->list (stx-cdr stx)))))
          (raise-syntax-error 'define-pvars "bad syntax" stx)
          (void))
      (let* ([pvars (reverse (syntax*->list (stx-cdr stx)))]
             [unique-at-runtime (map gensym (map syntax-e pvars))]
             [stxquoted-pvars+unique (map (λ (v unique)
                                            `(cons (quote-syntax ,v)
                                                   (quote-syntax ,unique)))
                                          pvars
                                          unique-at-runtime)])
        (datum->syntax
         (quote-syntax here)
         `(begin
            (define-values (,@unique-at-runtime)
              (values ,@(map (λ (pvar) `(gensym (quote ,pvar))) pvars)))
            (define-syntaxes ()
              (begin
                (pop-unreachable-pvars)
                (current-pvars-param
                 (list* ,@stxquoted-pvars+unique
                        (current-pvars-param)))
                (values)))))))))