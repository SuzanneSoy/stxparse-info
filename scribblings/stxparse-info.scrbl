#lang scribble/manual
@require[racket/require
         @for-label[stxparse-info/parse
                    stxparse-info/parse/experimental/template
                    stxparse-info/case
                    stxparse-info/current-pvars
                    (subtract-in racket/syntax stxparse-info/case)
                    (subtract-in racket/base stxparse-info/case)]
         version-case
         @for-syntax[racket/base]
         "ovl.rkt"]

@; Circumvent https://github.com/racket/scribble/issues/79
@(require scribble/struct
          scribble/decode)
@(define (nested-inset . vs)
   (nested #:style 'inset vs))

@(version-case 
  [(version< (version) "6.4")
   ]
  [else
   (require scribble/example)
   (define ev ((make-eval-factory '(racket))))])

@title{stxparse-info : Tracking bound syntax pattern variables}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

Source code: @url{https://github.com/jsmaniac/stxparse-info}

@defmodule[stxparse-info]

This library provides some patched versions of @orig:syntax-parse and of the
@orig:syntax-case family. These patched versions track which syntax pattern
variables are bound. This allows some libraries to change the way syntax
pattern variables work.

For example, @tt{subtemplate} automatically derives temporary
identifiers when a template contains @racket[yᵢ …], and @racket[xᵢ] is a
pattern variable. To know from which @racket[varᵢ] the @racket[yᵢ …]
identifiers must be derived, @tt{subtemplate} needs to know which
syntax pattern variables are within scope.

@section{Tracking currently-bound pattern variables with @racket[syntax-parse]}

@defmodule[stxparse-info/parse]

The module @racketmodname[stxparse-info/parse] provides patched versions of
@orig:syntax-parse, @orig:syntax-parser and @orig:define/syntax-parse which
track which syntax pattern variables are bound.

@(ovl syntax/parse
      syntax-parse
      syntax-parser
      define/syntax-parse)

Additionally, the following identifiers are overridden as they are part of the
duplicated implementation of @racketmodname[syntax/parse].

@(ovl #:wrapper nested-inset
      syntax/parse
      ...+
      attribute
      boolean
      char
      character
      define-conventions
      define-eh-alternative-set
      define-literal-set
      define-splicing-syntax-class
      define-syntax-class
      exact-integer
      exact-nonnegative-integer
      exact-positive-integer
      expr
      expr/c
      id
      identifier
      integer
      kernel-literals
      keyword
      literal-set->predicate
      nat
      number
      pattern
      static
      str
      this-syntax
      ~!
      ~and
      ~between
      ~bind
      ~commit
      ~datum
      ~delimit-cut
      ~describe
      ~do
      ~fail
      ~literal
      ~not
      ~once
      ~optional
      ~or
      ~parse
      ~peek
      ~peek-not
      ~post
      ~rest
      ~seq
      ~var)

@(version-case
  [(version>= (version) "6.9.0.6")
   (ovl #:wrapper nested-inset
        syntax/parse
        ~alt
        ~or*)]
  [else (begin)])

@(ovl #:wrapper nested-inset
      #:require (for-template syntax/parse)
      syntax/parse
      pattern-expander?
      pattern-expander
      prop:pattern-expander
      syntax-local-syntax-parse-pattern-introduce)

@section{Tracking currently-bound pattern variables with @racket[syntax-case]}

@defmodule[stxparse-info/case]

The module @racketmodname[stxparse-info/case] provides patched versions of
@orig:syntax-case, @orig:syntax-case*, @orig:with-syntax,
@orig:define/with-syntax, @orig:datum-case and @orig:with-datum which
track which syntax or datum pattern variables are bound.

@(ovl racket/base
      syntax-case
      syntax-case*
      with-syntax)

@(ovl syntax/datum
      datum-case
      with-datum)

@(ovl racket/syntax
      define/with-syntax)

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

@defproc[#:kind "procedure at phase 1"
         (current-pvars+unique) (listof (pairof identifier? identifier?))]{
 This for-syntax procedure works like @racket[current-pvars], but associates
 each syntax pattern variable with an identifier containing a unique symbol
 which is generated at each execution of the code recording the pattern
 variable via @racket[with-pvars] or @racket[define-pvars].

 The @racket[car] of each pair in the returned list is the syntax pattern
 variable (as produced by @racket[current-pvars]). It is the responsibility of
 the reader to check that the identifiers present in the @racket[car] of each
 element of the returned list are bound, and that they are bound to syntax
 pattern variables, for example using @racket[identifier-binding] and
 @racket[syntax-pattern-variable?]. This allows libraries to also track
 variables bound by match-like forms, for example.

 The @racket[cdr] of each pair is the identifier of a temporary variable.
 Reading that temporary variable produces a @racket[gensym]-ed symbol, which
 was generated at run-time at the point where @racket[with-pvars] or
 @racket[define-pvars] was used to record the corresponding pattern variable.

 This can be used to associate run-time data with each syntax pattern
 variable, via a weak hash table created with @racket[make-weak-hasheq]. For
 example, the @tt{subtemplate} library implicitly derives
 identifiers (similarly to @racket[generate-temporaries]) for uses of
 @racket[yᵢ ...] from a @racket[xᵢ] pattern variable bearing the same
 subscript. The generated identifiers are associated with @racket[xᵢ] via this
 weak hash table mechanism, so that two uses of @racket[yᵢ ...] within the
 scope of the same @racket[xᵢ] binding derive the same identifiers.

 The code @racket[(with-pvars (v) body)] roughly expands to:

 @racketblock[
 (let-values ([(tmp) (gensym 'v)])
   (letrec-syntaxes+values ([(shadow-current-pvars)
                             (list* (cons (quote-syntax v)
                                          (quote-syntax tmp))
                                    old-current-pvars)])
     body))]

 @bold{Caveat:} this entails that the fresh symbol stored in @racket[tmp] is
 generated when @racket[with-pvars] or @racket[define-pvars] is called, not
 when the syntax pattern variable is actually bound. For example:

 @RACKETBLOCK[
 (define-syntax (get-current-pvars+unique stx)
   #`'#,(current-pvars+unique))
              
 (require racket/private/sc)
 (let ([my-valvar (quote-syntax x)])
   (let-syntax ([my-pvar (make-syntax-mapping 0 (quote-syntax my-valvar))])
     (with-pvars (x)
       (get-current-pvars+unique)) (code:comment "'([x . g123])")
     (with-pvars (x)
       (get-current-pvars+unique)))) (code:comment "'([x . g124])")]

 Under normal circumstances, @racket[with-pvars] @racket[define-pvars] should
 be called immediately after binding the syntax pattern variable, but the code
 above shows that it is technically possible to do otherwise.

 This caveat is not meant to dissuade the use of
 @racket[current-pvars+unique], it rather serves as an explanation of the
 behaviour encountered when @racket[with-pvars] or @racket[define-pvars] are
 incorrectly used more than once to record the same pattern variable.}

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

@section{Extensions to @racketmodname[syntax/parse/experimental/template]}

@defmodule[stxparse-info/parse/experimental/template]

@(orig syntax/parse/experimental/template
       define-template-metafunction)

@defidform[define-template-metafunction]{
 Overloaded version of @orig:define-template-metafunction from
 @racketmodname[syntax/parse/experimental/template].

 Note that currently, template metafunctions defined via
 @racketmodname[stxparse-info/parse/experimental/template] are not compatible
 with the forms from @racketmodname[syntax/parse/experimental/template], and
 vice versa. There is a pending Pull Request which would make the necessary
 primitives from @racketmodname[syntax/parse/experimental/template] public, so
 hopefully this problem will be solved in future versions.}

@defform[(syntax-local-template-metafunction-introduce stx)]{
 Like @racket[syntax-local-introduce], but for
 @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{template metafunctions}.

 This change is also available in the package
 @racketmodname{backport-template-pr1514}. It has been submitted as a Pull
 Request to Racket, but can already be used in
 @racketmodname[stxparse-info/parse/experimental/template] right now.}

@(ovl syntax/parse/experimental/template
      template
      quasitemplate
      template/loc
      quasitemplate/loc)

Additionally, the following identifiers are overridden as they are part of the
duplicated implementation of @racketmodname[syntax/parse].

@(ovl #:wrapper nested-inset
      syntax/parse/experimental/template
      ??
      ?@)
