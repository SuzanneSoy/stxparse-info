#lang racket
(provide my-include)
(require version-case
         (for-syntax mzlib/etc))

(define-for-syntax (my-include1 esrcdir)
  (lambda (filename)
    (with-syntax ([esrcdir esrcdir]
                  [filename filename])
      #'(begin
          (define-syntax (tmp _stx)
            (my-include2 (this-expression-source-directory esrcdir) filename))
          (tmp)))))

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

(define-syntax (my-include stx)
  (syntax-case stx ()
    [(_ updir filename)
     (and (string? (syntax-e #'updir))
          (string? (syntax-e #'filename)))
     (let ([-updir (syntax-e #'updir)]
           [-filename (syntax-e #'filename)]
           [my-include1 (my-include1 #'filename)]
           [loc (lambda (x) (quasisyntax/loc #'filename #,x))])
       #`(version-case
           [(version< (version) "6.11.0.900")
             #,(my-include1 (loc (string-append -updir "6-11" -filename)))]
           [(version< (version) "6.90.0.29")
             #,(my-include1 (loc (string-append -updir "6-12" -filename)))]
           [(version< (version) "7.0.0.20")
             #,(my-include1 (loc (string-append -updir "6-90-0-29" -filename)))]
           [(version< (version) "7.3.0.1")
             #,(my-include1 (loc (string-append -updir "7-0-0-20" -filename)))]
           ;[(version< (version) "7.4")
           ;  #,(my-include1 (loc (string-append -updir "7-3-0-1" -filename)))]
           [(version< (version) "7.5")
             #,(my-include1 (loc (string-append -updir "7-4" -filename)))]
           [else
             #,(my-include1 (loc (string-append -updir "8-0" -filename)))]))]))
