#lang racket/base
(require (for-syntax racket/base
                     "dset.rkt"
                     racket/syntax
                     syntax/parse/private/minimatch
                     racket/private/stx ;; syntax/stx
                     racket/private/sc
                     racket/struct
                     auto-syntax-e/utils)
         stxparse-info/parse/private/residual
         racket/private/stx
         racket/performance-hint
         racket/private/promise)
(provide template
         template/loc
         quasitemplate
         quasitemplate/loc
         define-template-metafunction
         syntax-local-template-metafunction-introduce
         ??
         ?@
         (for-syntax template-metafunction?))

#|
To do:
- improve error messages
|#

#|
A Template (T) is one of:
  - pvar
  - const (including () and non-pvar identifiers)
  - (metafunction . T)
  - (H . T)
  - (H ... . T), (H ... ... . T), etc
  - (?? T T)
  - #(T*)
  - #s(prefab-struct-key T*)
  * (unquote expr)

A HeadTemplate (H) is one of:
  - T
  - (?? H)
  - (?? H H)
  - (?@ . T)
  * (unquote-splicing expr)
|#

(begin-for-syntax
 (define-logger template)

 ;; do-template : Syntax Syntax Boolean Id/#f -> Syntax
 (define (do-template ctx tstx quasi? loc-id)
   (with-disappeared-uses
     (parameterize ((current-syntax-context ctx)
                    (quasi (and quasi? (box null))))
       (define-values (guide pvars) (parse-template tstx loc-id))
       (define env (make-env pvars (hash)))
       (syntax-arm
        (with-syntax ([t tstx]
                      [((var . pvar-val-var) ...)
                       (for/list ([pvar (in-list pvars)])
                         (cons (hash-ref env pvar) (pvar-var pvar)))]
                      [((un-var . un-form) ...)
                       (if quasi? (reverse (unbox (quasi))) null)])
          #`(let ([un-var (handle-unsyntax un-form)] ... [var pvar-val-var] ...)
              (let ([tstx0 (quote-syntax t)])
                (#,(compile-guide guide env) tstx0))))))))

 ;; parse-template : Syntax Id/#f -> (values Guide (Listof PVar))
 (define (parse-template t loc-id)
   (define-values (drivers pre-guide) (parse-t t 0 #f))
   (define guide (if loc-id (relocate-guide pre-guide loc-id) pre-guide))
   (values guide (dset->list drivers)))

 ;; make-env : (Listof PVar) Hash[Pvar => Identifier] -> Hash[PVar => Identifier]
 (define (make-env pvars init-env)
   (for/fold ([env init-env]) ([pvar (in-list pvars)])
     (hash-set env pvar (car (generate-temporaries #'(pv_))))))
 )

(define-syntax (template stx)
  (syntax-case stx ()
    [(template t)
     (do-template stx #'t #f #f)]
    [(template t #:properties _)
     (begin
       (log-template-error "template #:properties argument no longer supported: ~e" stx)
       (do-template stx #'t #f #f))]))

(define-syntax (quasitemplate stx)
  (syntax-case stx ()
    [(quasitemplate t)
     (do-template stx #'t #t #f)]
    [(quasitemplate t #:properties (prop ...))
     (andmap identifier? (syntax->list #'(prop ...)))
     (parameterize ((props-to-serialize (syntax->datum #'(prop ...)))
                    (props-to-transfer (syntax->datum #'(prop ...))))
       ;; Same as above
       (do-template stx #'t #t #f))]))

(define-syntaxes (template/loc quasitemplate/loc)
  (let ([make-tx
         (lambda (quasi?)
           (lambda (stx)
             (syntax-case stx ()
               [(?/loc loc-expr t)
                (syntax-arm
                 (with-syntax ([main-expr (do-template stx #'t quasi? #'loc-stx)])
                   #'(let ([loc-stx (handle-loc '?/loc loc-expr)])
                       main-expr)))]
               [(?/loc loc-expr t #:properties (prop ...))
                (andmap identifier? (syntax->list #'(prop ...)))
                (parameterize ((props-to-serialize (syntax->datum #'(prop ...)))
                               (props-to-transfer (syntax->datum #'(prop ...))))
                  ;; Same as above
                  (syntax-arm
                   (with-syntax ([main-expr (do-template stx #'t quasi? #'loc-stx)])
                     #'(let ([loc-stx (handle-loc '?/loc loc-expr)])
                         main-expr))))])))])
    (values (make-tx #f) (make-tx #t))))

(define (handle-loc who x)
  (if (syntax? x)
      x
      (raise-argument-error who "syntax?" x)))

;; FIXME: what lexical context should result of expr get if not syntax?
(define-syntax handle-unsyntax
  (syntax-rules (unsyntax unsyntax-splicing)
    [(handle-unsyntax (unsyntax expr)) expr]
    [(handle-unsyntax (unsyntax-splicing expr)) expr]))

;; ----

(define-syntaxes (?? ?@)
  (let ([tx (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx))])
    (values tx tx)))

;; ============================================================

#|
See private/substitute for definition of Guide (G) and HeadGuide (HG).

A env-entry is (pvar syntax-mapping attribute-mapping/#f depth-delta)

The depth-delta associated with a depth>0 pattern variable is the difference
between the pattern variable's depth and the depth at which it is used. (For
depth 0 pvars, it's #f.) For example, in

  (with-syntax ([x #'0]
                [(y ...) #'(1 2)]
                [((z ...) ...) #'((a b) (c d))])
    (template (((x y) ...) ...)))

the depth-delta for x is #f, the depth-delta for y is 1, and the depth-delta for
z is 0. Coincidentally, the depth-delta is the same as the depth of the ellipsis
form at which the variable should be moved to the loop-env. That is, the
template above should be interpreted as roughly similar to

  (let ([x (pvar-value-of x)]
        [y (pvar-value-of y)]
        [z (pvar-value-of z)])
    (for ([Lz (in-list z)]) ;; depth 0
      (for ([Ly (in-list y)] ;; depth 1
            [Lz (in-list Lz)])
        (___ x Ly Lz ___))))

A Pre-Guide is like a Guide but with env-entry and (setof env-entry)
instead of integers and integer vectors.
|#

(begin-for-syntax
 (struct pvar (sm attr dd) #:prefab))

;; ============================================================


;; TODO: once PR https://github.com/racket/racket/pull/1591 is merged, use
;; the exported prop:template-metafunction, template-metafunction? and
;; template-metafunction-accessor.
(define-syntax (define-template-metafunction stx)
  (syntax-case stx ()
    [(dsm (id arg ...) . body)
     #'(dsm id (lambda (arg ...) . body))]
    [(dsm id expr)
     (identifier? #'id)
     (with-syntax ([(internal-id) (generate-temporaries #'(id))])
       #'(begin (define internal-id expr)
                (define-syntax id
                  (template-metafunction (quote-syntax internal-id)))))]))

(begin-for-syntax
 (struct template-metafunction (var)))

;; ============================================================

(begin-for-syntax

 ;; compile-guide : guide hash[env-entry => identifier] -> syntax[expr]
 (define (compile-guide g env)
   (define (lookup var) (hash-ref env var))
   (define (compile-t g in-try?)
     (define (loop g) (compile-t g in-try?))
     (define (loop-h g) (compile-h g in-try?))
     (match g
       ['_
        #`(t-const)]
       [(? pvar? pvar)
        (if (pvar-check? pvar)
            #`(t-check #,(lookup pvar) '#,in-try?)
            #`(t-var #,(lookup pvar)))]
       [(vector 'cons g1 g2)
        #`(t-cons #,(loop g1) #,(loop g2))]
       [(vector 'cons/p g1 g2)
        #`(t-cons/p #,(loop g1) #,(loop g2))]
       [(vector 'cons/x g1 g2)
        #`(t-cons/x #,(loop g1) #,(loop g2))]
       [(vector 'dots head new-driverss nesting tail)
        (let ()
          (define cons? (not (head-guide? head)))
          ;; AccElem = Stx if cons? is true, (Listof Stx) otherwise
          ;; gen-level : (Listof PVar) Syntax[(Listof AccElem) -> (Listof AccElem)]
          ;;          -> Syntax[(Listof AccElem) -> (Listof AccElem)]
          (define (gen-level vars inner)
            (with-syntax ([(var ...) (map lookup vars)]
                          [(var-value ...) (map var-value-expr vars)])
              #`(lambda (acc)
                  (let loop ([acc acc] [var var-value] ...)
                    (check-same-length var ...)
                    (if (and (pair? var) ...)
                        (loop (let ([var (car var)] ...)
                                (#,inner acc)) ;; inner has free refs to {var ...}
                              (cdr var) ...)
                        acc)))))
          ;; var-value-expr : PVar -> Syntax[List]
          (define (var-value-expr pvar)
            (with-syntax ([var (lookup pvar)])
              (if (pvar-check? pvar)
                  #`(check-list/depth stx var 1 '#,in-try?)
                  #'var)))
          (define head-loop-code
            (let nestloop ([new-driverss new-driverss] [old-drivers null])
              (cond [(null? new-driverss)
                     (if cons?
                         #`(lambda (acc) (cons (#,(loop head) stx) acc))
                         #`(lambda (acc) (cons (#,(loop-h head) stx) acc)))]
                    [else
                     (define drivers (append (car new-driverss) old-drivers))
                     (gen-level drivers (nestloop (cdr new-driverss) drivers))])))
          (if cons?
              #`(t-dots1 (lambda (stx) (#,head-loop-code null)) '#,nesting #,(loop tail))
              #`(t-dots  (lambda (stx) (#,head-loop-code null)) '#,nesting #,(loop tail))))]
       [(vector 'append/p head tail)
        #`(t-append/p #,(loop-h head) #,(loop tail))]
       [(vector 'append/x head tail)
        #`(t-append/x #,(loop-h head) #,(loop tail))]
       [(vector 'escaped g1)
        #`(t-escaped #,(loop g1))]
       [(vector 'orelse g1 g2)
        #`(t-orelse #,(compile-t g1 #t) #,(loop g2))]
       [(vector 'metafun mf g1)
        #`(t-metafun #,(template-metafunction-var mf) #,(loop g1))]
       [(vector 'vector g1)
        #`(t-vector #,(loop g1))]
       [(vector 'struct g1)
        #`(t-struct #,(loop g1))]
       [(vector 'box g1)
        #`(t-box #,(loop g1))]
       [(vector 'unsyntax var)
        #`(t-unsyntax #,var)]
       [(vector 'relocate g1 var)
        #`(t-relocate #,(loop g1) #,var)]
       [else (error 'template "internal error: bad guide: ~e" g)]))
   (define (compile-h g in-try?)
     (define (loop g) (compile-t g in-try?))
     (define (loop-h g) (compile-h g in-try?))
     (match g
       [(vector 'orelse-h1 g1)
        #`(t-orelse #,(compile-h g1 #t) #f)]
       [(vector 'orelse-h g1 g2)
        #`(t-orelse #,(compile-h g1 #t) #,(loop-h g2))]
       [(vector 'splice g1)
        #`(t-splice #,(loop g1))]
       [(vector 'unsyntax-splicing var)
        #`(t-unsyntax-splicing #,var)]
       [else #`(t-h #,(loop g))]))
   (compile-t g #f))

 (define (head-guide? x)
   (match x
     [(vector 'orelse-h1 g) #t]
     [(vector 'splice g) #t]
     [(vector 'orelse-h g1 g2) #t]
     [(vector 'unsyntax-splicing var) #t]
     [_ #f]))

 ;; ----------------------------------------

 ;; relocate-guide : guide pvar -> guide
 (define (relocate-guide g0 loc-pvar)
   (define (relocate g)
     (vector 'relocate g loc-pvar))
   (define (error/no-relocate)
     (wrong-syntax #f "cannot apply syntax location to template"))
   (define (loop g)
     (match g
       ['_
        (relocate g)]
       [(vector 'cons g1 g2)
        (relocate g)]
       [(vector 'cons/x g1 g2)
        (relocate g)]
       [(? pvar? g)
        g]
       [(vector 'dots head new-hdrivers/level nesting tail)
        ;; Ideally, should error. For perfect backwards compatability,
        ;; should relocate. But if there are zero iterations, that
        ;; means we'd relocate tail (which might be bad). Making
        ;; relocation depend on number of iterations would be
        ;; complicated. So just ignore.
        g]
       [(vector 'escaped g1)
        (vector 'escaped (loop g1))]
       [(vector 'vector g1)
        (relocate g)]
       [(vector 'struct g1)
        (relocate g)]
       [(vector 'box g1)
        (relocate g)]
       [(vector 'unsyntax var)
        g]
       ;; ----
       [(vector 'append/x ghead gtail)
        (match ghead
          [(vector 'unsyntax-splicing _) g]
          [_ (error/no-relocate)])]
       ;; ----
       [(vector 'orelse g1 g2)
        (error/no-relocate)]
       [(vector 'orelse-h g1 g2)
        (error/no-relocate)]
       [(vector 'metafun mf g1)
        (error/no-relocate)]
       [(vector 'orelse-h1 g1)
        (error/no-relocate)]
       [(vector 'splice g1)
        (error/no-relocate)]
       [(vector 'unsyntax-splicing var)
        g]
       [else (error 'template "internal error: bad guide for relocation: ~e" g0)]))
   (loop g0))

 ;; ----------------------------------------

 ;; quasi : (parameterof (or/c #f (list^n (boxof QuasiPairs))))
 ;; each list wrapper represents nested quasi wrapping
 ;; QuasiPairs = (listof (cons/c identifier syntax))
 (define quasi (make-parameter #f))

 (define (stx-dots? x) (and (identifier? x) (free-identifier=? x (quote-syntax ...))))

 (define (cons-guide g1 g2)
   (if (and (eq? g1 '_) (eq? g2 '_)) '_ (vector 'cons g1 g2)))
 (define (cons/p-guide g1 g2)
   (if (and (eq? g1 '_) (eq? g2 '_)) '_ (vector 'cons/p g1 g2)))
 (define (cons/x-guide g1 g2)
   (if (and (eq? g1 '_) (eq? g2 '_)) '_ (vector 'cons/x g1 g2)))

 (define (list-guide . gs) (foldr cons-guide '_ gs))
 (define (list/p-guide . gs) (foldr cons/p-guide '_ gs))
 (define (list/x-guide . gs) (foldr cons/x-guide '_ gs))

 ;; parse-t : stx nat boolean -> (values (dsetof env-entry) pre-guide)
 (define (parse-t t depth esc?)
   (cond [(stx-pair? t)
          (if (identifier? (stx-car t))
              (parse-t-pair/command t depth esc?)
              (parse-t-pair/dots t depth esc?))]
         [else (parse-t-nonpair t depth esc?)]))

 ;; parse-t-pair/command : Stx Nat Boolean -> ...
 ;; t is a stxpair w/ id in car; check if it is a "command" (metafun, escape, etc)
 (define (parse-t-pair/command t depth esc?)
   (syntax-case t (quasitemplate unsyntax ??)
     [(quasitemplate template)
      (quasi)
      (parameterize ((quasi (list (quasi))))
        (let-values ([(drivers guide) (parse-t #'template depth esc?)])
          (values drivers (list-guide '_ guide))))]
     [(unsyntax e)
      (quasi)
      (let ([qval (quasi)])
        (cond [(box? qval)
               (with-syntax ([(tmp) (generate-temporaries #'(unsyntax-expr))])
                 (set-box! qval (cons (cons #'tmp t) (unbox qval)))
                 (values (dset) (vector 'unsyntax #'tmp)))]
              [else
               (parameterize ((quasi (car qval)))
                 (let-values ([(drivers guide) (parse-t #'e depth esc?)])
                   (values drivers (list-guide '_ guide))))]))]
     [(DOTS template)
      (and (not esc?) (free-identifier=? #'DOTS (quote-syntax ...)))
      (let-values ([(drivers guide) (parse-t #'template depth #t)])
        (values drivers (vector 'escaped guide)))]
     [(?? t1 t2)
      (not esc?)
      (let-values ([(drivers1 guide1) (parse-t #'t1 depth esc?)]
                   [(drivers2 guide2) (parse-t #'t2 depth esc?)])
        (values (dset-union drivers1 drivers2) (vector 'orelse guide1 guide2)))]
     [(mf . _)
      (and (not esc?) (template-metafunction? (lookup #'mf #f)))
      (let-values ([(mf) (lookup #'mf #f)]
                   [(drivers guide) (parse-t (stx-cdr t) depth esc?)])
        (values drivers (vector 'metafun mf guide)))]
     [_ (parse-t-pair/dots t depth esc?)]))

 ;; parse-t-pair/dots : Stx Nat Boolean -> ...
 ;; t is a stx pair; check for dots
 (define (parse-t-pair/dots t depth esc?)
   (define head (stx-car t))
   (define-values (tail nesting)
     (let loop ([tail (stx-cdr t)] [nesting 0])
       (if (and (not esc?) (stx-pair? tail) (stx-dots? (stx-car tail)))
           (loop (stx-cdr tail) (add1 nesting))
           (values tail nesting))))
   (if (zero? nesting)
       (parse-t-pair/normal t depth esc?)
       (let-values ([(hdrivers hguide) (parse-h head (+ depth nesting) esc?)]
                    [(tdrivers tguide) (parse-t tail depth esc?)])
         (when (dset-empty? hdrivers)
           (wrong-syntax head "no pattern variables before ellipsis in template"))
         (when (dset-empty? (dset-filter hdrivers (pvar/dd<=? depth)))
           (let ([bad-dots ;; select the nestingth (last) ellipsis as the bad one
                  (stx-car (stx-drop nesting t))])
             ;; FIXME: improve error message?
             (wrong-syntax bad-dots "too many ellipses in template")))
         (values (dset-union hdrivers tdrivers)
                 ;; hdrivers is (listof (dsetof pvar)); compute pvars new to each level
                 (let* ([hdrivers/level
                         (for/list ([i (in-range nesting)])
                           (dset-filter hdrivers (pvar/dd<=? (+ depth i))))]
                        [new-hdrivers/level
                         (let loop ([raw hdrivers/level] [last (dset)])
                           (cond [(null? raw) null]
                                 [else
                                  (cons (dset->list (dset-subtract (car raw) last))
                                        (loop (cdr raw) (car raw)))]))])
                   (vector 'dots hguide new-hdrivers/level nesting tguide))))))

 ;; parse-t-pair/normal : Stx Nat Boolean -> ...
 ;; t is a normal stx pair
 (define (parse-t-pair/normal t depth esc?)
   (define-values (hdrivers hguide) (parse-h (stx-car t) depth esc?))
   (define-values (tdrivers tguide) (parse-t (stx-cdr t) depth esc?))
   (values (dset-union hdrivers tdrivers)
           (let ([kind (if (head-guide? hguide)
                           (if (syntax? t) 'append/x 'append/p)
                           (if (syntax? t) 'cons/x 'cons/p))])
             (vector kind hguide tguide))))

 ;; parse-t-nonpair : Stx Nat Boolean -> ...
 ;; PRE: t is not a stxpair
 (define (parse-t-nonpair t depth esc?)
   (syntax-case t (?? ?@ unsyntax quasitemplate)
     [id
      (identifier? #'id)
      (cond [(or (and (not esc?)
                      (or (free-identifier=? #'id (quote-syntax ...))
                          (free-identifier=? #'id (quote-syntax ??))
                          (free-identifier=? #'id (quote-syntax ?@))))
                 (and (quasi)
                      (or (free-identifier=? #'id (quote-syntax unsyntax))
                          (free-identifier=? #'id (quote-syntax unsyntax-splicing)))))
             (wrong-syntax #'id "illegal use")]
            [else
             (let ([pvar (lookup #'id depth)])
               (cond [(pvar? pvar)
                      (values (dset pvar) pvar)]
                     [(template-metafunction? pvar)
                      (wrong-syntax t "illegal use of syntax metafunction")]
                     [else
                      (values (dset) '_)]))])]
     [vec
      (vector? (syntax-e #'vec))
      (let-values ([(drivers guide)
                    (parse-t (vector->list (syntax-e #'vec)) depth esc?)])
        (values drivers (if (eq? guide '_) '_ (vector 'vector guide))))]
     [pstruct
      (prefab-struct-key (syntax-e #'pstruct))
      (let-values ([(drivers guide)
                    (parse-t (cdr (vector->list (struct->vector (syntax-e #'pstruct)))) depth esc?)])
        (values drivers (if (eq? guide '_) '_ (vector 'struct guide))))]
     [#&template
      (let-values ([(drivers guide)
                    (parse-t #'template depth esc?)])
        (values drivers (if (eq? guide '_) '_ (vector 'box guide))))]
     [const
      (values (dset) '_)]))

 ;; parse-h : Syntax Nat Boolean -> (values (dsetof PVar) HeadGuide)
 (define (parse-h h depth esc?)
   (syntax-case h (?? ?@ unsyntax-splicing)
     [(?? t)
      (not esc?)
      (let-values ([(drivers guide) (parse-h #'t depth esc?)])
        (values drivers (vector 'orelse-h1 guide)))]
     [(?? t1 t2)
      (not esc?)
      (let-values ([(drivers1 guide1) (parse-h #'t1 depth esc?)]
                   [(drivers2 guide2) (parse-h #'t2 depth esc?)])
        (values (dset-union drivers1 drivers2)
                (if (or (head-guide? guide1) (head-guide? guide2))
                    (vector 'orelse-h guide1 guide2)
                    (vector 'orelse   guide1 guide2))))]
     [(?@ . _)
      (not esc?)
      (let-values ([(drivers guide) (parse-t (stx-cdr h) depth esc?)])
        (values drivers (vector 'splice guide)))]
     [(unsyntax-splicing t1)
      (quasi)
      (let ([qval (quasi)])
        (cond [(box? qval)
               (with-syntax ([(tmp) (generate-temporaries #'(unsyntax-splicing-expr))])
                 (set-box! qval (cons (cons #'tmp h) (unbox qval)))
                 (values (dset) (vector 'unsyntax-splicing #'tmp)))]
              [else
               (parameterize ((quasi (car qval)))
                 (let*-values ([(drivers guide) (parse-t #'t1 depth esc?)]
                               [(drivers guide) (values drivers (list-guide '_ guide))])
                   (values drivers guide)))]))]
     [t
      (let-values ([(drivers guide) (parse-t #'t depth esc?)])
        (values drivers guide))]))

 (define (lookup id depth)
   (let ([v (syntax-local-value/record id (lambda (v) (or (syntax-pattern-variable? v)
                                                          (template-metafunction? v))))])
     (cond [(syntax-pattern-variable? v)
            (let* ([pvar-depth (syntax-mapping-depth v)]
                   [attr (syntax-local-value (syntax-mapping-valvar v) (lambda () #f))]
                   [attr (and (attribute-mapping? attr) attr)])
              (cond [(not depth) ;; not looking for pvars, only for metafuns
                     #f]
                    [(zero? pvar-depth)
                     (pvar v attr #f)]
                    [(>= depth pvar-depth)
                     (pvar v attr (- depth pvar-depth))]
                    [else
                     (wrong-syntax id "missing ellipses with pattern variable in template")]))]
           [(template-metafunction? v)
            v]
           [else
            ;; id is a literal; check that for all x s.t. id = x.y, x is not an attribute
            (for ([pfx (in-list (dotted-prefixes id))])
              (let ([pfx-v (syntax-local-value pfx (lambda () #f))])
                (when (and (syntax-pattern-variable? pfx-v)
                           (let ([valvar (syntax-mapping-valvar pfx-v)])
                             (attribute-mapping? (syntax-local-value valvar (lambda () #f)))))
                  (wrong-syntax id "undefined nested attribute of attribute `~a'" (syntax-e pfx)))))
            #f])))

 (define (dotted-prefixes id)
   (let* ([id-string (symbol->string (syntax-e id))]
          [dot-locations (map car (regexp-match-positions* #rx"\\.[^.]" id-string))])
     (for/list ([loc (in-list dot-locations)])
       (datum->syntax id (string->symbol (substring id-string 0 loc))))))

 (define (index-hash->vector hash [f values])
   (let ([vec (make-vector (hash-count hash))])
     (for ([(value index) (in-hash hash)])
       (vector-set! vec (sub1 index) (f value)))
     vec))

 (define ((pvar/dd<=? expected-dd) x)
   (match x
     [(pvar sm attr dd) (and dd (<= dd expected-dd))]
     [_ #f]))

 (define (pvar-var x)
   (match x
     [(pvar sm '#f dd) (syntax-mapping-valvar sm)]
     [(pvar sm attr dd) (attribute-mapping-var attr)]))

 (define (pvar-check? x)
   (match x
     [(pvar sm '#f dd) #f]
     [(pvar sm attr dd) (not (attribute-mapping-syntax? attr))]))

 (define (stx-drop n x) (for/fold ([x x]) ([i (in-range n)]) (stx-cdr x)))
 )

;; ============================================================

#|
A Guide (G) is one of:
  - '_                       ;; constant
  - PVar                     ;; pattern variable
  - (vector 'cons G G)       ;; template is pair or syntax-pair => restx, use stx-{car,cdr}
  - (vector 'cons/p G G)     ;; template is non-syntax pair => no restx, use {car,cdr}
  - (vector 'cons/x G G)     ;; template is syntax-pair => restx, use {car,cdr}+syntax-e
  - (vector 'vector G)
  - (vector 'struct G)
  - (vector 'box G)
  - (vector 'dots HG (listof (listof PVar)) Nat G)
  - (vector 'append/p HG G)  ;; template is non-syntax pair => no restx, use {car,cdr}
  - (vector 'append/x HG G)  ;; template is syntax-pair => restx, use {car,cdr}+syntax-e
  - (vector 'escaped G)
  - (vector 'orelse G G)
  - (vector 'metafun Metafunction G)
  - (vector 'unsyntax Id)
  - (vector 'relocate G Id)

A HeadGuide (HG) is one of:
  - G
  - (vector 'orelse-h1 H)
  - (vector 'orelse-h H H)
  - (vector 'splice G)
  - (vector 'unsyntax-splicing Id)
|#

(begin-encourage-inline
(define ((t-const) stx) stx)
(define ((t-var v) stx) v)
(define ((t-check v in-try?) stx) (check-stx stx v in-try?))
(define ((t-append/p h t) stx) (append (h (car stx)) (t (cdr stx))))
(define ((t-append/x h t) stx) (restx stx (append (h (car (syntax-e stx))) (t (cdr (syntax-e stx))))))
(define ((t-cons h t) stx) (restx stx (cons (h (stx-car stx)) (t (stx-cdr stx)))))
(define ((t-cons/p h t) stx) (cons (h (car stx)) (t (cdr stx))))
(define ((t-cons/x h t) stx) (restx stx (cons (h (car (syntax-e stx))) (t (cdr (syntax-e stx))))))
(define ((t-dots h n t) stx)
  (restx stx (revappend* (h (stx-car stx)) (t (stx-drop (add1 n) stx)))))
(define ((t-dots1 h n t) stx)
  (restx stx (revappend (h (stx-car stx)) (t (stx-drop (add1 n) stx)))))
(define ((t-escaped g) stx) (g (stx-cadr stx)))
(define ((t-orelse g1 g2) stx)
  (with-handlers ([absent-pvar? (lambda (e) (if g2 (g2 (stx-caddr stx)) null))])
    (g1 (stx-cadr stx))))
(define ((t-vector g) stx) (restx stx (list->vector (g (vector->list (syntax-e stx))))))
(define ((t-box g) stx) (restx stx (box (g (unbox (syntax-e stx))))))
(define ((t-struct g) stx)
  (define s (syntax-e stx))
  (define key (prefab-struct-key s))
  (define elems (cdr (vector->list (struct->vector s))))
  (restx stx (apply make-prefab-struct key (g elems))))
(define ((t-h g) stx) (list (g stx)))
)

(define ((t-metafun mf g) stx)
  (define v (restx stx (cons (stx-car stx) (g (stx-cdr stx)))))
  (define mark (make-syntax-introducer))
  (define old-mark (current-template-metafunction-introducer))
  (parameterize ((current-template-metafunction-introducer mark)
                 (old-template-metafunction-introducer old-mark))
    (define r (call-with-continuation-barrier (lambda () (mf (mark (old-mark v))))))
    (unless (syntax? r)
      (raise-syntax-error #f "result of template metafunction was not syntax" stx))
    (old-mark (mark r))))
(define ((t-splice g) stx)
  (let ([r (g (stx-cdr stx))])
    (or (stx->list r)
        (raise-syntax-error 'template "splicing template did not produce a syntax list" stx))))
(define ((t-unsyntax v) stx) (restx stx v))
(define ((t-unsyntax-splicing v) stx) (stx->list v))
(define ((t-relocate g loc) stx)
  (define new-stx (g stx))
  (datum->syntax new-stx (syntax-e new-stx) loc new-stx))

(begin-encourage-inline
(define (stx-cadr x) (stx-car (stx-cdr x)))
(define (stx-cddr x) (stx-cdr (stx-cdr x)))
(define (stx-caddr x) (stx-car (stx-cdr (stx-cdr x))))
(define (stx-drop n x) (for/fold ([x x]) ([i (in-range n)]) (stx-cdr x)))
(define (restx basis val)
  (if (syntax? basis) (datum->syntax basis val basis basis) val))
)

;; revappend* : (Listof (Listof X)) (Listof X) -> (Listof X)
(define (revappend* xss ys)
  (if (null? xss) ys (revappend* (cdr xss) (append (car xss) ys))))

;; revappend : (Listof X) (Listof X) -> (Listof X)
(define (revappend xs ys)
  (if (null? xs) ys (revappend (cdr xs) (cons (car xs) ys))))

(define current-template-metafunction-introducer
  (make-parameter (lambda (stx) (if (syntax-transforming?) (syntax-local-introduce stx) stx))))

(define old-template-metafunction-introducer
  (make-parameter #f))

(define (syntax-local-template-metafunction-introduce stx)
  (let ([mark (current-template-metafunction-introducer)]
        [old-mark (old-template-metafunction-introducer)])
    (unless old-mark
      (error 'syntax-local-template-metafunction-introduce
             "must be called within the dynamic extent of a template metafunction"))
    (mark (old-mark stx))))

;; Used to indicate absent pvar in template; ?? catches
;; Note: not an exn, don't need continuation marks
#;(require (only-in rackunit require/expose))
#;(require/expose syntax/parse/experimental/private/substitute
                  (absent-pvar
                   absent-pvar?
                   absent-pvar-ctx
                   absent-pvar-v
                   absent-pvar-wanted-list?))
;; this struct is only used in this file, and is not exported, so I guess it's
;; ok to not steal the struct from syntax/parse/experimental/private/substitute
;; Furthermore, the require/expose above does not work reliably.
(struct absent-pvar (ctx))

(define (check-stx ctx v in-try?)
  (cond [(syntax? v) v]
        [(promise? v) (check-stx ctx (force v) in-try?)]
        [(and in-try? (eq? v #f)) (raise (absent-pvar ctx))]
        [else (err/not-syntax ctx v)]))

(define (check-list/depth ctx v0 depth0 in-try?)
  (let depthloop ([v v0] [depth depth0])
    (cond [(zero? depth) v]
          [(and (= depth 1) (list? v)) v]
          [else
           (let loop ([v v])
             (cond [(null? v)
                    null]
                   [(pair? v)
                    (let ([new-car (depthloop (car v) (sub1 depth))]
                          [new-cdr (loop (cdr v))])
                      ;; Don't copy unless necessary
                      (if (and (eq? new-car (car v)) (eq? new-cdr (cdr v)))
                          v
                          (cons new-car new-cdr)))]
                   [(promise? v)
                    (loop (force v))]
                   [(and in-try? (eq? v #f))
                    (raise (absent-pvar ctx))]
                   [else (err/not-syntax ctx v0)]))])))

;; FIXME: use raise-syntax-error instead, pass stx args
(define check-same-length
  (case-lambda
    [(a) (void)]
    [(a b)
     (unless (= (length a) (length b))
       (error 'syntax "incompatible ellipsis match counts for template"))]
    [(a . bs)
     (define alen (length a))
     (for ([b (in-list bs)])
       (unless (= alen (length b))
         (error 'template "incompatible ellipsis match counts for template")))]))

;; Note: slightly different from error msg in syntax/parse/private/residual:
;; here says "contains" instead of "is bound to", because might be within list
(define (err/not-syntax ctx v)
  (raise-syntax-error #f (format "attribute contains non-syntax value\n  value: ~e" v) ctx))
