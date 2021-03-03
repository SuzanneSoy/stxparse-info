#lang info
(define collection "stxparse-info")
(define deps '(("base" #:version "6.7.0.900")
               "rackunit-lib"
               ;; Because scribble/example is not available on v6.3:
               "version-case"
               "subtemplate" ;; for the documentation only
               "auto-syntax-e"
               "compatibility-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "at-exp-lib")) ;; for the documentation only
(define scribblings '(("scribblings/stxparse-info.scrbl" () ("Syntax Extensions"))))
(define compile-omit-paths '("6-11" "6-12" "6-90-0-29" "7-0-0-20" "7-3-0-1" "8-0"))
(define test-omit-paths '("6-11" "6-12" "6-90-0-29" "7-0-0-20" "7-3-0-1" "8-0"))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(Suzanne Soy))

