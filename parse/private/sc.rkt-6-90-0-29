#lang racket/base
(require racket/lazy-require
         syntax/parse/private/keywords
         "residual.rkt")

(lazy-require-syntax
 ["parse.rkt"
  (define-syntax-class
   define-splicing-syntax-class
   define-integrable-syntax-class
   syntax-parse
   syntax-parser
   define/syntax-parse
   syntax-parser/template
   parser/rhs
   define-eh-alternative-set)])

(provide define-syntax-class
         define-splicing-syntax-class
         define-integrable-syntax-class
         syntax-parse
         syntax-parser
         define/syntax-parse

         (except-out (all-from-out syntax/parse/private/keywords)
                     ~reflect
                     ~splicing-reflect
                     ~eh-var)
         attribute
         this-syntax

         syntax-parser/template
         parser/rhs
         define-eh-alternative-set)
