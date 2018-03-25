#lang racket
(require stxparse-info/parse
         stxparse-info/case
         stxparse-info/current-pvars
         racket/stxparam
         rackunit
         syntax/macro-testing
         (for-syntax racket/list))

;; Test utilities
(define-syntax (list-pvars stx)
  #`'#,(current-pvars))

(define-syntax (list-pvars+unique-id stx)
  #`'#,(current-pvars+unique))

(define-syntax (list-pvars+unique-val stx)
  (with-syntax ([([pv . un] ...) (current-pvars+unique)])
    #`(list (cons 'pv un) ...)))

;; Check that the identifier has the right scopes
(define-syntax (ref-nth-pvar stx)
  (syntax-case stx ()
    [(_ n)
     (number? (syntax-e #'n))
     #`#'#,(let ([pvar (if (>= (syntax-e #'n) (length (current-pvars)))
                           #'too-big!
                           (list-ref (current-pvars) (syntax-e #'n)))])
             (datum->syntax pvar (syntax-e pvar) stx))]))

;; First check that (current-pvars) returns the empty list before anything
;; is done:

(check-equal? (list-pvars)
              '())

(let ()
  (define/with-syntax x #'1)
  (void))

(check-equal? (list-pvars)
              '())

;; test that the x is correctly removed, even if no querry was made
;; between its creation and the creation of the y.
(let () (define/with-syntax x #'1) (void))
(let ()
  (define/with-syntax y #'2)
  (check-equal? (list-pvars)
                '(y))
  (void))

(check-equal? (list (list-pvars)
                    (syntax-case #'() ()
                      [() (list (list-pvars)
                                (syntax-case #'(1 2 3 a b c) ()
                                  [(x y ...)
                                   (list-pvars)])
                                (list-pvars))])
                    (list-pvars))
              '(() (() (y x) ()) ()))

(check-equal? (list (list-pvars)
                    (syntax-case #'(-1 -2) ()
                      [(k l) (list (list-pvars)
                                   (syntax-case #'(1 2 3 a b c) ()
                                     [(z t ...)
                                      (list-pvars)])
                                   (list-pvars))])
                    (list-pvars))
              '(() ((l k) (t z l k) (l k)) ()))

;; Simple case:
(check-equal? (syntax-parse #'(1 2 3 a b c)
                [(x y ...)
                 (list-pvars)])
              '(y x))

;; Simple case:
(check-equal? (syntax-case #'() ()
                [() (syntax-parse #'(1 2 3 a b c)
                      [(x y ...)
                       (list-pvars)])])
              '(y x))

;; Mixed definitions from user code and from a macro
(begin
  (define-syntax (mixed stx)
    (syntax-case stx ()
      [(_ val def body)
       #'(let ()
           (define/syntax-parse x #'val)
           def
           body)]))

  (check-equal? (mixed 1 (define/syntax-parse y #'2)
                       (mixed 3 (define/syntax-parse y #'4)
                              (list-pvars)))
                '(y x y x))

  (check-equal? (mixed 1 (define/syntax-parse y #'2)
                       (mixed 3 (define/syntax-parse y #'4)
                              (list (syntax->datum (ref-nth-pvar 0))
                                    (syntax->datum (ref-nth-pvar 1))
                                    (syntax->datum (ref-nth-pvar 2))
                                    (syntax->datum (ref-nth-pvar 3)))))
                '(4 3 2 1)))

(check-equal? (list-pvars)
              '())

;; Tests for syntax-parse
(begin
  (check-equal? (syntax-parse #'(1 2 3 a b c)
                  [(x y:nat ... {~parse w (list-pvars)} z ...)
                   (syntax->datum #`[w #,(list-pvars)])])
                '([y x] [z w y x]))

  (check-equal? (list-pvars)
                '())

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
                '(1 2 1)))

;; Tests for syntax-case
(begin
  (check-equal? (list-pvars)
                '())

  (check-equal? (syntax-case #'(1 (2 3) a b c) ()
                  [(_ ...)
                   (list-pvars)])
                '())

  (check-equal? (syntax-case #'(1 (2 3) a b c) ()
                  [(x (y ...) z ...)
                   (list-pvars)])
                '(z y x))

  (check-equal? (list-pvars)
                '())

  (check-equal? (syntax-case #'(x) ()
                  [(_)
                   (list-pvars)])
                '())

  (check-equal? (syntax-case #'() ()
                  [()
                   (list-pvars)])
                '())

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
                '(1 2 1)))

;; tests for define/syntax-parse and define/syntax-case
(define-syntax-rule (gen-test-define define/xxx)
  (...
   (begin
     (check-equal? (syntax-parse #'1
                     [_
                      (list (list-pvars)
                            (let ()
                              (define/xxx z #'3)
                              (list-pvars)))])
                   '(() (z)))

     (check-equal? (syntax-parse #'1
                     [_
                      (syntax-parse #'2
                        [_
                         (list-pvars)])])
                   '())

     (check-equal? (let ()
                     (define/xxx _ #'1)
                     (list-pvars))
                   '())

     (check-equal? (let ()
                     (define/xxx (_ ...) #'(1 2 3))
                     (list-pvars))
                   '())
    
     (check-equal? (syntax-parse #'1
                     [x
                      #:with y #'2
                      (define/xxx z #'3)
                      (list-pvars)])
                   '(z y x))

     (check-equal? (syntax-parse #'1
                     [x
                      #:with y #'2
                      (define/xxx z #'3)
                      (list (syntax->datum (ref-nth-pvar 0))
                            (syntax->datum (ref-nth-pvar 1))
                            (syntax->datum (ref-nth-pvar 2)))])
                   '(3 2 1))

     (check-equal? (syntax-parse #'1
                     [x
                      #:with y #'2
                      (define/xxx x #'3)
                      (list-pvars)])
                   '(x y x))

     (check-equal? (syntax-parse #'1
                     [x
                      #:with (y ...) #'(2 3)
                      (define/xxx (x ...) #'(4 5))
                      (list-pvars)])
                   '(x y x))

     (check-equal? (syntax-parse #'1
                     [x
                      #:with y #'2
                      (define/xxx x #'3)
                      (list (syntax->datum (ref-nth-pvar 0))
                            (syntax->datum (ref-nth-pvar 1))
                            (syntax->datum (ref-nth-pvar 2)))])
                   '(3 2 1))

     (check-equal? (syntax-parse #'1
                     [x
                      #:with y #'2
                      (define/xxx x #'3)
                      (define/xxx y #'4)
                      (list (syntax->datum (ref-nth-pvar 0))
                            (syntax->datum (ref-nth-pvar 1))
                            (syntax->datum (ref-nth-pvar 2))
                            (syntax->datum (ref-nth-pvar 3)))])
                   '(4 3 2 1))
  
     (check-equal? (syntax-parse #'1
                     [x
                      #:with y #'2
                      (define/xxx x #'3)
                      (define/xxx y #'4)
                      (define/xxx z #'5)
                      (list (syntax->datum (ref-nth-pvar 0))
                            (syntax->datum (ref-nth-pvar 1))
                            (syntax->datum (ref-nth-pvar 2))
                            (syntax->datum (ref-nth-pvar 3))
                            (syntax->datum (ref-nth-pvar 4)))])
                   '(5 4 3 2 1))

     (check-equal? (syntax-parse #'(1 2 3)
                     [(x y z)
                      (define/xxx x #'4)
                      (define/xxx y #'5)
                      (list (syntax->datum (ref-nth-pvar 0))
                            (syntax->datum (ref-nth-pvar 1))
                            (syntax->datum (ref-nth-pvar 2))
                            (syntax->datum (ref-nth-pvar 3))
                            (syntax->datum (ref-nth-pvar 4)))])
                   '(5 4 3 2 1))

     (check-equal? (syntax-parse #'(1 2 3)
                     [(x y z)
                      (define/xxx x #'4)
                      (define/xxx y #'5)
                      (list-pvars)])
                   '(y x z y x))

     ;; Test with nested let, less variables in the nested let
     (check-equal? (let ()
                     (define/xxx w #'1)
                     (define/xxx x #'2)
                     (define/xxx y #'3)
                     (define/xxx z #'4)
                     (list (list-pvars)
                           (let ()
                             (define/xxx w #'5)
                             (define/xxx x #'6)
                             (list-pvars))
                           (list-pvars)))
                   '((z y x w) (x w z y x w) (z y x w)))

     ;; Test with nested let, more variables in the nested let
     (check-equal? (let ()
                     (define/xxx w #'1)
                     (define/xxx x #'2)
                     (list (list-pvars)
                           (let ()
                             (define/xxx w #'3)
                             (define/xxx x #'4)
                             (define/xxx y #'5)
                             (define/xxx z #'6)
                             (list-pvars))
                           (list-pvars)))
                   '((x w) (z y x w x w) (x w)))

     (check-equal? (let ()
                     (define/xxx w #'1)
                     (define/xxx x #'2)
                     (define/xxx y #'3)
                     (define/xxx z #'4)
                     (list (list-pvars)
                           (syntax-parse #'5
                             [k
                              (define/xxx w #'5)
                              (define/xxx x #'6)
                              (list-pvars)])
                           (list-pvars)))
                   '((z y x w) (x w k z y x w) (z y x w)))

     (check-equal? (let ()
                     (define/xxx w #'1)
                     (define/xxx x #'2)
                     (list (list-pvars)
                           (syntax-parse #'5
                             [k
                              (define/xxx w #'3)
                              (define/xxx x #'4)
                              (define/xxx y #'5)
                              (define/xxx z #'6)
                              (list-pvars)])
                           (list-pvars)))
                   '((x w) (z y x w k x w) (x w)))

     (check-equal? (let ()
                     (define/xxx w #'1)
                     (define/xxx x #'2)
                     (list (list-pvars)
                           (syntax-parse #'5
                             [k
                              (define/xxx w #'3)
                              (define/xxx x #'4)
                              (define/xxx y #'5)
                              (define/xxx z #'6)
                              (list (list-pvars)
                                    (syntax-parse #'5
                                      [k
                                       (define/xxx x #'4)
                                       (define/xxx y #'4)
                                       (list-pvars)])
                                    (list-pvars))])
                           (list-pvars)))
                   '((x w)
                     ((z y x w k x w)
                      (y x k z y x w k x w)
                      (z y x w k x w))
                     (x w))))))
(gen-test-define define/syntax-parse)
(gen-test-define define/with-syntax)

(check-exn #rx"bad syntax"
           (λ ()
             (convert-compile-time-error
              (with-pvars a 'body))))

(check-exn #rx"bad syntax"
           (λ ()
             (convert-compile-time-error
              (with-pvars ((a)) 'body))))

(check-exn #rx"bad syntax"
           (λ ()
             (convert-compile-time-error
              (with-pvars ((a) b) 'body))))

(check-exn #rx"bad syntax"
           (λ ()
             (convert-compile-time-error
              (with-pvars (a) 'body1 . 2))))

(check-exn #rx"bad syntax"
           (λ ()
             (convert-compile-time-error
              (let ()
                (define-pvars (a))))))

(check-exn #rx"bad syntax"
           (λ ()
             (convert-compile-time-error
              (let ()
                (define-pvars (a) b)))))

(check-exn #rx"bad syntax"
           (λ ()
             (convert-compile-time-error
              (let ()
                (define-pvars a . 2)))))

(check-true (match (syntax-case #'(1 2 3) ()
                     [(x ... y)
                      (list-pvars+unique-id)])
              [(list (cons 'y (? symbol?))
                     (cons 'x (? symbol?)))
               #true]
              [_
               #false]))

(let ()
  (define/with-syntax (x ... y) #'(1 2 3))
  (check-true (match (list-pvars+unique-val)
                [(list (cons 'y (? symbol?))
                       (cons 'x (? symbol?)))
                 #true]
                [v
                 (displayln v)
                 #false])))

(check-true (match (syntax-case #'(1 2 3) ()
                     [(x ... y)
                      (list-pvars+unique-val)])
              [(list (cons 'y (? symbol?))
                     (cons 'x (? symbol?)))
               #true]
              [_
               #false]))

(check-equal? (match (map (λ (v)
                            (syntax-case v ()
                              [(x ... y)
                               (list-pvars+unique-id)])) ;; ID
                          (list #'(a b c) #'(d)))
                [(list (list (cons 'y (? symbol? y-unique1))
                             (cons 'x (? symbol? x-unique1)))
                       (list (cons 'y (? symbol? y-unique2))
                             (cons 'x (? symbol? x-unique2))))
                 (list (eq? y-unique1 y-unique1)
                       (eq? y-unique1 x-unique1)
                       (eq? y-unique1 y-unique2)
                       (eq? y-unique1 x-unique2)
                       
                       (eq? x-unique1 y-unique1)
                       (eq? x-unique1 x-unique1)
                       (eq? x-unique1 y-unique2)
                       (eq? x-unique1 x-unique2)

                       (eq? y-unique2 y-unique1)
                       (eq? y-unique2 x-unique1)
                       (eq? y-unique2 y-unique2)
                       (eq? y-unique2 x-unique2)
                       
                       (eq? x-unique2 y-unique1)
                       (eq? x-unique2 x-unique1)
                       (eq? x-unique2 y-unique2)
                       (eq? x-unique2 x-unique2))]
                [_
                 #false])
              (list #t #f #t #f
                    #f #t #f #t
                    #t #f #t #f
                    #f #t #f #t))

(check-equal? (match (map (λ (v)
                            (syntax-case v ()
                              [(x ... y)
                               (list-pvars+unique-val)])) ;; VAL
                          (list #'(a b c) #'(d)))
                [(list (list (cons 'y (? symbol? y-unique1))
                             (cons 'x (? symbol? x-unique1)))
                       (list (cons 'y (? symbol? y-unique2))
                             (cons 'x (? symbol? x-unique2))))
                 (list (eq? y-unique1 y-unique1)
                       (eq? y-unique1 x-unique1)
                       (eq? y-unique1 y-unique2)
                       (eq? y-unique1 x-unique2)
                       
                       (eq? x-unique1 y-unique1)
                       (eq? x-unique1 x-unique1)
                       (eq? x-unique1 y-unique2)
                       (eq? x-unique1 x-unique2)

                       (eq? y-unique2 y-unique1)
                       (eq? y-unique2 x-unique1)
                       (eq? y-unique2 y-unique2)
                       (eq? y-unique2 x-unique2)
                       
                       (eq? x-unique2 y-unique1)
                       (eq? x-unique2 x-unique1)
                       (eq? x-unique2 y-unique2)
                       (eq? x-unique2 x-unique2))]
                [_
                 #false])
              (list #t #f #f #f
                    #f #t #f #f
                    #f #f #t #f
                    #f #f #f #t))

(check-equal? (syntax-case #'(1 2 3) ()
                [(_ ... _)
                 (list-pvars+unique-id)])
              '())

(check-equal? (syntax-case #'(1 2 3) ()
                [(_ ... _)
                 (list-pvars+unique-val)])
              '())

;; stress-test the binary tree implementation
(define-syntax-rule (defs1 pv ...)
  (let ()
    (define/with-syntax pv #'12321)
    ...
    (list-pvars)))

(define-syntax (check-defs1 stx)
  (syntax-case stx ()
    [(_ n)
     (with-syntax ([(pv ...) (map (λ (_) (gensym))
                                  (range (syntax-e #'n)))])
       #'(check-equal? (reverse (defs1 pv ...)) '(pv ...)))]))

(define-syntax (check-defs1* stx)
  (syntax-case stx ()
    [(_ start end)
     (with-syntax ([(nᵢ ...) (range (syntax-e #'start) (syntax-e #'end))])
       #'(begin
           (check-defs1 nᵢ)
           ...))]))

(check-equal? (reverse (defs1)) '())
(check-equal? (reverse (defs1 a)) '(a))
(check-equal? (reverse (defs1 a b)) '(a b))
(check-equal? (reverse (defs1 a b c)) '(a b c))
(check-equal? (reverse (defs1 a b c d)) '(a b c d))
(check-equal? (reverse (defs1 a b c d e)) '(a b c d e))
(check-defs1* 6 65) ;; continue tests with 6 till 65 pvars

(define-syntax-rule (defs2 pv ...)
  (let ()
    (define/with-syntax xyz #'12300)
    (define/with-syntax pv #'12321)
    ...
    (define/with-syntax www #'12399)
    (let ()
      (define/with-syntax pv #'12321)
      ...
      (list-pvars))))

(define-syntax (check-defs2 stx)
  (syntax-case stx ()
    [(_ n)
     (with-syntax ([(pv ...) (map (λ (_) (gensym))
                                  (range (syntax-e #'n)))])
       #'(check-equal? (reverse (defs2 pv ...)) '(xyz pv ... www pv ...)))]))

(define-syntax (check-defs2* stx)
  (syntax-case stx ()
    [(_ start end)
     (with-syntax ([(nᵢ ...) (range (syntax-e #'start) (syntax-e #'end))])
       #'(begin
           (check-defs2 nᵢ)
           ...))]))

(check-equal? (reverse (defs2)) '(xyz www))
(check-equal? (reverse (defs2 a)) '(xyz a www a))
(check-equal? (reverse (defs2 a b)) '(xyz a b www a b))
(check-equal? (reverse (defs2 a b c)) '(xyz a b c www a b c))
(check-equal? (reverse (defs2 a b c d)) '(xyz a b c d www a b c d))
(check-equal? (reverse (defs2 a b c d e)) '(xyz a b c d e www a b c d e))
(check-defs2* 6 65) ;; continue tests with 6 till 65 pvars

(define-syntax (defs3 stx)
  (syntax-case stx ()
    [(_)
     #'(list (list-pvars))]
    [(_ pv₀ pvᵢ ...)
     #'(cons (list-pvars)
             (let ()
               (define/with-syntax pv₀ #'12321)
               (defs3 pvᵢ ...)))]))

(define-syntax (*expected-defs3 stx)
  (syntax-case stx ()
    [(_)
     #'(list '())]
    [(_ pvᵢ ... pvₙ)
     #'(cons '(pvᵢ ... pvₙ)
             (*expected-defs3 pvᵢ ...))]))
(define-syntax-rule (expected-defs3 pv ...)
  (reverse (*expected-defs3 pv ...)))

(define-syntax (check-defs3 stx)
    (syntax-case stx ()
      [(_ n)
       (with-syntax ([(pv ...) (map (λ (_) (gensym))
                                    (range (syntax-e #'n)))])
         #'(check-equal? (map reverse (defs3 pv ...))
                         (expected-defs3 pv ...)))]))

(define-syntax (check-defs3* stx)
    (syntax-case stx ()
      [(_ start end)
       (with-syntax ([(nᵢ ...) (range (syntax-e #'start) (syntax-e #'end))])
         #'(begin
             (check-defs3 nᵢ)
             ...))]))

(check-equal? (map reverse (defs3)) '(()))
(check-equal? (map reverse (defs3 a)) '(() (a)))
(check-equal? (map reverse (defs3 a b)) '(() (a) (a b)))
(check-equal? (map reverse (defs3 a b c)) '(() (a) (a b) (a b c)))
(check-equal? (map reverse (defs3 a b c d)) '(() (a) (a b) (a b c) (a b c d)))
(check-equal? (map reverse (defs3 a b c d e))
              '(() (a) (a b) (a b c) (a b c d) (a b c d e)))

(check-equal? (expected-defs3) '(()))
(check-equal? (expected-defs3 a) '(() (a)))
(check-equal? (expected-defs3 a b) '(() (a) (a b)))
(check-equal? (expected-defs3 a b c) '(() (a) (a b) (a b c)))
(check-equal? (expected-defs3 a b c d) '(() (a) (a b) (a b c) (a b c d)))
(check-equal? (expected-defs3 a b c d e)
              '(() (a) (a b) (a b c) (a b c d) (a b c d e)))

(check-defs3* 6 65) ;; continue tests with 6 till 65 pvars

(check-equal? (list-pvars)
              '())