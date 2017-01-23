#lang info
(define collection "stxparse-info")
(define deps '("base"
               "rackunit-lib"
               ;; Because scribble/example is not available on v6.3:
               "version-case"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/stxparse-info.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
