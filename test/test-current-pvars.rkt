#lang racket
(require stxparse-info/parse
         stxparse-info/current-pvars
         racket/stxparam
         rackunit)

;; Test utilities
(define-syntax (list-pvars stx)
  #`'#,(current-pvars))

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

;; Simple case:
(check-equal? (syntax-parse #'(1 2 3 a b c)
                [(x y ...)
                 (list-pvars)])
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

;; tests for define/syntax-parse
(begin
  (check-equal? (syntax-parse #'1
                  [x
                   #:with y #'2
                   (define/syntax-parse z #'3)
                   (list-pvars)])
                '(z y x))

  (check-equal? (syntax-parse #'1
                  [x
                   #:with y #'2
                   (define/syntax-parse z #'3)
                   (list (syntax->datum (ref-nth-pvar 0))
                         (syntax->datum (ref-nth-pvar 1))
                         (syntax->datum (ref-nth-pvar 2)))])
                '(3 2 1))

  (check-equal? (syntax-parse #'1
                  [x
                   #:with y #'2
                   (define/syntax-parse x #'3)
                   (list-pvars)])
                '(x y x))

  (check-equal? (syntax-parse #'1
                  [x
                   #:with y #'2
                   (define/syntax-parse x #'3)
                   (list (syntax->datum (ref-nth-pvar 0))
                         (syntax->datum (ref-nth-pvar 1))
                         (syntax->datum (ref-nth-pvar 2)))])
                '(3 2 1))

  (check-equal? (syntax-parse #'1
                  [x
                   #:with y #'2
                   (define/syntax-parse x #'3)
                   (define/syntax-parse y #'4)
                   (list (syntax->datum (ref-nth-pvar 0))
                         (syntax->datum (ref-nth-pvar 1))
                         (syntax->datum (ref-nth-pvar 2))
                         (syntax->datum (ref-nth-pvar 3)))])
                '(4 3 2 1))
  
  (check-equal? (syntax-parse #'1
                  [x
                   #:with y #'2
                   (define/syntax-parse x #'3)
                   (define/syntax-parse y #'4)
                   (define/syntax-parse z #'5)
                   (list (syntax->datum (ref-nth-pvar 0))
                         (syntax->datum (ref-nth-pvar 1))
                         (syntax->datum (ref-nth-pvar 2))
                         (syntax->datum (ref-nth-pvar 3))
                         (syntax->datum (ref-nth-pvar 4)))])
                '(5 4 3 2 1))

  (check-equal? (syntax-parse #'(1 2 3)
                  [(x y z)
                   (define/syntax-parse x #'4)
                   (define/syntax-parse y #'5)
                   (list (syntax->datum (ref-nth-pvar 0))
                         (syntax->datum (ref-nth-pvar 1))
                         (syntax->datum (ref-nth-pvar 2))
                         (syntax->datum (ref-nth-pvar 3))
                         (syntax->datum (ref-nth-pvar 4)))])
                '(5 4 3 2 1))

  (check-equal? (syntax-parse #'(1 2 3)
                  [(x y z)
                   (define/syntax-parse x #'4)
                   (define/syntax-parse y #'5)
                   (list-pvars)])
                '(y x z y x))

  ;; Test with nested let, less variables in the nested let
  (check-equal? (let ()
                  (define/syntax-parse w #'1)
                  (define/syntax-parse x #'2)
                  (define/syntax-parse y #'3)
                  (define/syntax-parse z #'4)
                  (list (list-pvars)
                        (let ()
                          (define/syntax-parse w #'5)
                          (define/syntax-parse x #'6)
                          (list-pvars))
                        (list-pvars)))
                '((z y x w) (x w z y x w) (z y x w)))

  ;; Test with nested let, more variables in the nested let
  (check-equal? (let ()
                  (define/syntax-parse w #'1)
                  (define/syntax-parse x #'2)
                  (list (list-pvars)
                        (let ()
                          (define/syntax-parse w #'3)
                          (define/syntax-parse x #'4)
                          (define/syntax-parse y #'5)
                          (define/syntax-parse z #'6)
                          (list-pvars))
                        (list-pvars)))
                '((x w) (z y x w x w) (x w)))

  (check-equal? (let ()
                  (define/syntax-parse w #'1)
                  (define/syntax-parse x #'2)
                  (define/syntax-parse y #'3)
                  (define/syntax-parse z #'4)
                  (list (list-pvars)
                        (syntax-parse #'5
                          [k
                           (define/syntax-parse w #'5)
                           (define/syntax-parse x #'6)
                           (list-pvars)])
                        (list-pvars)))
                '((z y x w) (x w k z y x w) (z y x w)))

  (check-equal? (let ()
                  (define/syntax-parse w #'1)
                  (define/syntax-parse x #'2)
                  (list (list-pvars)
                        (syntax-parse #'5
                          [k
                           (define/syntax-parse w #'3)
                           (define/syntax-parse x #'4)
                           (define/syntax-parse y #'5)
                           (define/syntax-parse z #'6)
                           (list-pvars)])
                        (list-pvars)))
                '((x w) (z y x w k x w) (x w)))

  (check-equal? (let ()
                  (define/syntax-parse w #'1)
                  (define/syntax-parse x #'2)
                  (list (list-pvars)
                        (syntax-parse #'5
                          [k
                           (define/syntax-parse w #'3)
                           (define/syntax-parse x #'4)
                           (define/syntax-parse y #'5)
                           (define/syntax-parse z #'6)
                           (list (list-pvars)
                                 (syntax-parse #'5
                                   [k
                                    (define/syntax-parse x #'4)
                                    (define/syntax-parse y #'4)
                                    (list-pvars)])
                                 (list-pvars))])
                        (list-pvars)))
                '((x w)
                  ((z y x w k x w)
                   (y x k z y x w k x w)
                   (z y x w k x w))
                  (x w))))
