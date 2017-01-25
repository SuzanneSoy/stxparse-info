#lang scribble/manual
@require[@for-label[stxparse-info/parse
                    stxparse-info/current-pvars
                    racket/syntax
                    racket/base]
         version-case
         @for-syntax[racket/base]]

@(version-case 
  [(version< (version) "6.4")
   ]
  [else
   (require scribble/example)
   (define ev ((make-eval-factory '(racket))))])

@title{stxparse-info : Tracking bound syntax pattern variables with
 @racket[syntax-parse] and @racket[syntax-case]}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

Source code: @url{https://github.com/jsmaniac/stxparse-info}

@defmodule[stxparse-info]

This library provides some patched versions of @racket[syntax-parse]
and the @racket[syntax-case] family. These patched versions track which syntax
pattern variables are bound. This allows some libraries to change the way
syntax pattern variables work.

For example, @racketmodname[phc-graph/subtemplate] automatically derives
temporary identifiers when a template contains @racket[yᵢ …], and @racket[xᵢ]
is a pattern variable. To know from which @racket[varᵢ] the @racket[yᵢ …]
identifiers must be derived, @racketmodname[phc-graph/subtemplate] needs to
know which syntax pattern variables are within scope.

@section{Tracking currently-bound pattern variables with @racket[syntax-parse]}

@defmodule[stxparse-info/parse]

The module @racketmodname[stxparse-info/parse] provides patched versions of
@racketmodname[syntax/parse] @racketmodname[define/syntax-parse] which track
which syntax pattern variables are bound.

@section{Tracking currently-bound pattern variables with @racket[syntax-case]}

@defmodule[stxparse-info/case]

The module @racketmodname[stxparse-info/case] provides patched versions of
@racket[syntax-case], @racket[syntax-case*], @racket[with-syntax],
@racket[define/with-syntax], @racket[datum-case] and @racket[with-datum] which
track which syntax or datum pattern variables are bound.

@section{Reading and updating the list of currently-bound pattern variables}

@defmodule[stxparse-info/current-pvars] 

@defproc[#:kind "procedure at phase 1"
         (current-pvars) (listof identifier?)]{
 This for-syntax procedure returns the list of syntax pattern variables which
 are known to be bound. The most recently bound variables are at the beginning
 of the list.

 It is the responsibility of the reader to check that the identifiers are
 bound, and that they are bound to syntax pattern variables, for example using
 @racket[identifier-binding] and @racket[syntax-pattern-variable?]. This allows
 libraries to also track variables bound by match-like forms, for example.}

@defform[(with-pvars (pvar ...) . body)
         #:contracts ([pvar identifier?])]{
 Prepends the given @racket[pvar ...] to the list of pattern variables which
 are known to be bound. The @racket[pvar ...] are prepended in reverse order,
 so within the body of

 @racketblock[(with-pvars (v₁ v₂ v₃) . body)]
 
 a call to the for-syntax function @racket[(current-pvars)] returns:

 @racketblock[(list* (quote-syntax v₃) (quote-syntax v₂) (quote-syntax v₁)
                     old-current-pvars)]

 This can be used to implement macros which work similarly to
 @racket[syntax-parse] or @racket[syntax-case], and have them record the syntax
 pattern variables which they bind.

 Note that the identifiers @racket[pvar ...] must already be bound to syntax
 pattern variables when @racket[with-pvars] is used, e.g.

 @racketblock[
 (let-syntax ([v₁ (make-syntax-mapping depth (quote-syntax valvar))]
              [v₂ (make-syntax-mapping depth (quote-syntax valvar))])
   (with-pvars (v₁ v₂)
               code))]

 instead of:

 @racketblock[
 (with-pvars (v₁ v₂)
             (let-syntax ([v₁ (make-syntax-mapping depth (quote-syntax valvar))]
                          [v₂ (make-syntax-mapping depth (quote-syntax valvar))])
               code))]}

@defform[(define-pvars pvar ...)
         #:contracts ([pvar identifier?])]{
                                           
 Prepends the given @racket[pvar ...] to the list of pattern variables which
 are known to be bound, in the same way as @racket[with-pvars]. Whereas
 @racket[with-pvars] makes the modified list visible in the @racket[_body],
 @racket[define-pvars] makes the modified list visible in the statements
 following @racket[define-pvars]. @racket[define-pvars] can be used multiple
 times within the same @racket[let] or equivalent.

 This can be used to implement macros which work similarly to
 @racket[define/syntax-parse] or @racket[define/with-syntax], and have them
 record the syntax pattern variables which they bind.

 @(version-case 
   [(version< (version) "6.4")
    @RACKETBLOCK[
 (let ()
   (code:comment "Alternate version of define/syntax-parse which")
   (code:comment "contains (define-pvars x) in its expanded form.")
   (define/syntax-parse x #'1)
   (define/syntax-parse y #'2)
   (define-syntax (get-pvars stx)
     #`'#,(current-pvars))
   (get-pvars))
 (code:comment "=> '(y x)")]]
   [else
    @examples[
 #:eval ev
 #:hidden
 (require stxparse-info/parse
          stxparse-info/current-pvars
          racket/syntax
          (for-syntax racket/base))]
 
    @examples[
 #:eval ev
 #:escape UNSYNTAX
 (eval:check
  (let ()
    (code:comment "Alternate version of define/syntax-parse which")
    (code:comment "contains (define-pvars x) in its expanded form.")
    (define/syntax-parse x #'1)
    (define/syntax-parse y #'2)
    (define-syntax (get-pvars stx)
      #`'#,(current-pvars))
    (get-pvars))
  '(y x))]])}