#lang scribble/manual
@require[@for-label[stxparse-info/parse
                    stxparse-info/current-pvars
                    racket/base]]

@title{stxparse-info : tracking bound syntax pattern variables with
 @racketmodname[syntax/parse]}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@defmodule[stxparse-info/parse]

The module @racketmodname[stxparse-info/parse] is a patched version of
@racketmodname[syntax/parse] which tracks which syntax pattern variables are
bound. This allows some libraries to change the way syntax pattern variables
work.

For example, @racketmodname[phc-graph/subtemplate] automatically derives
temporary identifiers when a template contains @racket[yᵢ …], and @racket[xᵢ]
is a pattern variable. To know from which @racket[varᵢ] the @racket[yᵢ …]
identifiers must be derived, @racketmodname[phc-graph/subtemplate] needs to
know which syntax pattern variables are within scope.

@section{Reading and updating the list of currently-bound pattern variables}

@defmodule[stxparse-info/current-pvars] 

@defproc[#:kind "procedure at phase 1"
         (current-pvars) (listof identifier?)]{
 This for-syntax procedure returns the list of syntax pattern variables which
 are known to be bound. The most recently bound variables are at the beginning
 of the list.}

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