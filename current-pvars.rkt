(module current-pvars '#%kernel
  (#%provide (for-syntax current-pvars)
             with-pvars
             define-pvars)
  
  (#%require racket/private/small-scheme
             (for-syntax '#%kernel
                         racket/private/qq-and-or
                         racket/private/stx))

  ;; This is a poor man's syntax parameter. Since the implementation of
  ;; racket/stxparam depends on syntax-case, and we want to add current-pvars to
  ;; syntax-case, we cannot use syntax parameters, lest we create a cyclic
  ;; dependency. Instead, we implement here a simplified "syntax parameter".
  ;  Like racket/stxparam, it relies on nested bindings of the same identifier,
  ;; and on syntax-local-get-shadower to access the most nested binding.

  ;; Since define/with-syntax and define/syntax-parse need to add new ids to
  ;; the list, they redefine current-pvars-param, shadowing the outer binding.
  ;; Unfortunately, if a let form contains two uses of define/with-syntax, this
  ;; would result in two redefinitions of current-pvars-param, which would cause
  ;; a "duplicate definition" error. Instead of shadowing the outer bindings, we
  ;; therefore store the list of bound syntax pattern variables in a new, fresh
  ;; identifier. When accessing the list, (current-pvars) then checks all such
  ;; identifiers. The identifiers have the form current-pvars-paramNNN and are
  ;; numbered sequentially, each new "shadowing" identifier using the number
  ;; following the latest visible identifier.
  ;; When it is safe to shadow identifiers (i.e. for with-pvars, but not for
  ;; define-pvars), current-pvars-index-lower-bound is also shadowed.
  ;; When current-pvars-index-lower-bound is bound, it contains the index of the
  ;; latest current-pvars-paramNNN at that point.
  ;; When accessing the latest current-pvars-paramNNN, a dichotomy search is
  ;; performed between current-pvars-index-lower-bound and an upper bound
  ;; computed by trying to access lower-bound + 2ᵏ, with increasing values of k,
  ;; until an unbound identifier is found.

  ;; (poor-man-parameterof exact-nonnegative-integer?)
  (define-syntaxes (current-pvars-index-lower-bound) 0)
  ;; (poor-man-parameterof (listof identifier?))
  (define-syntaxes (current-pvars-param0) '())

  (begin-for-syntax
    ;; (-> identifier? (or/c #f (listof identifier?)))
    (define-values (try-current-pvars)
      (λ (id)
        (syntax-local-value
         (syntax-local-get-shadower id
                                    #t)
         ;; Default value if we are outside of any with-pvars.
         (λ () #f))))

    ;; (-> exact-nonnegative-integer? identifier?)
    (define-values (nth-current-pvars-id)
      (λ (n)
        (syntax-local-introduce
         (datum->syntax (quote-syntax here)
                        (string->symbol
                         (format "current-pvars-param~a" n))))))
    
    ;; (-> exact-nonnegative-integer? (or/c #f (listof identifier?)))
    (define-values (try-nth-current-pvars)
      (λ (n)
        (try-current-pvars (nth-current-pvars-id n))))

    ;; (-> exact-nonnegative-integer? exact-nonnegative-integer?
    ;;     exact-nonnegative-integer?)
    ;; Doubles the value of n until (+ start n) is not a valid index
    ;; in the current-pvars-param pseudo-array
    (define-values (double-max)
      (λ (start n)
        (if (try-nth-current-pvars (+ start n))
            (double-max start (* n 2))
            (+ start n))))


    ;; (-> exact-nonnegative-integer? exact-nonnegative-integer?
    ;;     exact-nonnegative-integer?)
    ;; Preconditions: upper > lower ∧ upper - lower = 2ᵏ ∧ k ∈ ℕ
    ;; Returns the last valid index in the current-pvars-param pseudo-array,
    ;; by dichotomy between 
    (define-values (dichotomy)
      (λ (lower upper)
        (if (= (- upper lower) 1)
            (if (try-nth-current-pvars upper)
                upper
                lower)
            (let ([mid (/ (+ upper lower) 2)])
              (if (try-nth-current-pvars mid)
                  (dichotomy mid upper)
                  (dichotomy lower mid))))))

    ;; (-> exact-nonnegative-integer?)
    (define-values (find-last-current-pvars)
      (λ ()
        (let ([lower-bound (syntax-local-value
                            (syntax-local-get-shadower
                             (syntax-local-introduce
                              (quote-syntax current-pvars-index-lower-bound))
                             #t))])
          (if (not (try-nth-current-pvars (+ lower-bound 1)))
              ;; Short path for the common case where there are no uses
              ;; of define/with-syntax or define/syntax-parse in the most nested
              ;; syntax-case, with-syntax or syntax-parse
              lower-bound
              ;; Find an upper bound by repeatedly doubling an offset (starting
              ;; with 1) from the lower bound, then perform a dichotomy between
              ;; these two bounds.
              (dichotomy lower-bound
                         (double-max lower-bound 1))))))

    ;; (-> (listof identifier?))
    (define-values (current-pvars)
      (λ ()
        (try-nth-current-pvars (find-last-current-pvars)))))

  ;; (with-pvars [pvar ...] . body)
  (define-syntaxes (with-pvars)
    (lambda (stx)
      (if (not (and (stx-pair? stx)
                    (identifier? (stx-car stx))
                    (stx-pair? (stx-cdr stx))
                    (syntax->list (stx-car (stx-cdr stx)))
                    (andmap identifier?
                            (syntax->list (stx-car (stx-cdr stx))))))
          (raise-syntax-error 'with-pvars "bad syntax" stx)
          (void))
      (let* ([pvars (syntax->list (stx-car (stx-cdr stx)))]
             [quoted-pvars (map (λ (v) `(quote-syntax ,v)) pvars)]
             [body (stx-cdr (stx-cdr stx))]
             [old-pvars-index (find-last-current-pvars)]
             [old-pvars (try-nth-current-pvars old-pvars-index)]
             [binding (syntax-local-identifier-as-binding
                       (nth-current-pvars-id (+ old-pvars-index 1)))]
             [lower-bound-binding
              (syntax-local-identifier-as-binding
               (syntax-local-introduce
                (quote-syntax current-pvars-index-lower-bound)))])
        (datum->syntax
         (quote-syntax here)
         `(letrec-syntaxes+values
              ([(,binding) (list* ,@quoted-pvars
                                  (try-nth-current-pvars ,old-pvars-index))]
               [(,lower-bound-binding) ,(+ old-pvars-index 1)])
              ()
            . ,body)))))

  (define-syntaxes (define-pvars)
    (lambda (stx)
      (if (not (and (stx-pair? stx)
                    (identifier? (stx-car stx))
                    (syntax->list (stx-cdr stx))
                    (andmap identifier?
                            (syntax->list (stx-cdr stx)))))
          (raise-syntax-error 'with-pvars "bad syntax" stx)
          (void))
      (let* ([pvars (syntax->list (stx-cdr stx))]
             [quoted-pvars (map (λ (v) `(quote-syntax ,v)) pvars)]
             [old-pvars-index (find-last-current-pvars)]
             [old-pvars (try-nth-current-pvars old-pvars-index)]
             [binding (syntax-local-identifier-as-binding
                       (nth-current-pvars-id (+ old-pvars-index 1)))])
        (datum->syntax
         (quote-syntax here)
         `(define-syntaxes (,binding)
            (list* ,@quoted-pvars
                   (try-nth-current-pvars ,old-pvars-index))))))))