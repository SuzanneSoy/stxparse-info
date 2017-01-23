(module case '#%kernel
  (#%require "case/stxloc.rkt"
             "case/syntax.rkt"
             "case/with-stx.rkt")
  (#%provide syntax-case
             syntax-case*
             define/with-syntax
             with-syntax
             datum-case
             with-datum))