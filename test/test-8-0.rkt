#lang racket/base

(require (only-in syntax/parse/experimental/template ?? template)
         (only-in syntax/parse #;syntax-parse #;attribute)
         (only-in "../parse.rkt" syntax-parse ~optional nat))

(syntax-parse #'(#:kw)
  [({~optional z:nat} _)
   (template (x (?? z) y))])