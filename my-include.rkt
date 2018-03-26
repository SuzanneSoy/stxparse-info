#lang racket
(provide my-include)
(require (for-syntax mzlib/etc))

(define-syntax (my-include stx)
  (syntax-case stx ()
    [(_ filename)
     (string? (syntax-e #'filename))
     #'(begin
         (define-syntax (tmp _stx)
           (my-include2 (this-expression-source-directory filename) filename))
         (tmp))]))

(define-for-syntax (my-include2 dirname filename)
  (let ([filename (build-path dirname
                              filename)])
    (define s
      (parameterize ([read-accept-reader #t])
        (read-syntax filename (open-input-file filename))))
    (syntax-case s ()
      [(-module name . rest)
       #'(begin (module name . rest)
                (require 'name)
                (provide (all-from-out 'name)))])))