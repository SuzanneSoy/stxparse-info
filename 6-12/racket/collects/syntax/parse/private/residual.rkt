#lang racket/base
(require (for-syntax racket/base)
         racket/stxparam
         racket/lazy-require
         racket/private/promise)

;; ============================================================
;; Compile-time

(require (for-syntax racket/private/sc syntax/parse/private/residual-ct))
(provide (for-syntax (all-from-out syntax/parse/private/residual-ct)))

(begin-for-syntax
  ;; == from runtime.rkt

 (provide make-attribute-mapping
          attribute-mapping?
          attribute-mapping-var
          attribute-mapping-name
          attribute-mapping-depth
          attribute-mapping-syntax?)

 (require (only-in (for-template syntax/parse/private/residual)
                   make-attribute-mapping
                   attribute-mapping?
                   attribute-mapping-var
                   attribute-mapping-name
                   attribute-mapping-depth
                   attribute-mapping-syntax?))
 #;(define-struct attribute-mapping (var name depth syntax?)
   #:omit-define-syntaxes
   #:property prop:procedure
   (lambda (self stx)
     (if (attribute-mapping-syntax? self)
         #`(#%expression #,(attribute-mapping-var self))
         (let ([source-name
                (or (let loop ([p (syntax-property stx 'disappeared-use)])
                      (cond [(identifier? p) p]
                            [(pair? p) (or (loop (car p)) (loop (cdr p)))]
                            [else #f]))
                    (attribute-mapping-name self))])
           #`(let ([value #,(attribute-mapping-var self)])
               (if (syntax-list^depth? '#,(attribute-mapping-depth self) value)
                   value
                   (check/force-syntax-list^depth '#,(attribute-mapping-depth self)
                                                  value
                                                  (quote-syntax #,source-name))))))))
 )

;; ============================================================
;; Run-time

(require "runtime-progress.rkt"
         "3d-stx.rkt"
         auto-syntax-e
         syntax/stx
         stxparse-info/current-pvars)

(provide (all-from-out "runtime-progress.rkt")

         this-syntax
         this-role
         this-context-syntax
         attribute
         attribute-binding
         stx-list-take
         stx-list-drop/cx
         datum->syntax/with-clause
         check/force-syntax-list^depth
         check-literal*
         error/null-eh-match
         begin-for-syntax/once

         name->too-few/once
         name->too-few
         name->too-many
         normalize-context
         syntax-patterns-fail)

;; == from runtime.rkt

;; this-syntax
;; Bound to syntax being matched inside of syntax class
(define-syntax-parameter this-syntax
  (lambda (stx)
    (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

(define-syntax-parameter this-role
  (lambda (stx)
    (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

;; this-context-syntax
;; Bound to (expression that extracts) context syntax (bottom frame in progress)
(define-syntax-parameter this-context-syntax
  (lambda (stx)
    (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

(define-syntax (attribute stx)
  (syntax-case stx ()
    [(attribute name)
     (identifier? #'name)
     (let ([mapping (syntax-local-value #'name (lambda () #f))])
       (unless (syntax-pattern-variable? mapping)
         (raise-syntax-error #f "not bound as a pattern variable" stx #'name))
       (let ([var (syntax-mapping-valvar mapping)])
         (let ([attr (syntax-local-value var (lambda () #f))])
           (unless (attribute-mapping? attr)
             (raise-syntax-error #f "not bound as an attribute" stx #'name))
           (syntax-property (attribute-mapping-var attr)
                            'disappeared-use
                            (list (syntax-local-introduce #'name))))))]))

;; (attribute-binding id)
;; mostly for debugging/testing
(define-syntax (attribute-binding stx)
  (syntax-case stx ()
    [(attribute-bound? name)
     (identifier? #'name)
     (let ([value (syntax-local-value #'name (lambda () #f))])
       (if (syntax-pattern-variable? value)
           (let ([value (syntax-local-value (syntax-mapping-valvar value) (lambda () #f))])
             (if (attribute-mapping? value)
                 #`(quote #,(make-attr (attribute-mapping-name value)
                                       (attribute-mapping-depth value)
                                       (attribute-mapping-syntax? value)))
                 #'(quote #f)))
           #'(quote #f)))]))

;; stx-list-take : stxish nat -> syntax
(define (stx-list-take stx n)
  (datum->syntax #f
                 (let loop ([stx stx] [n n])
                   (if (zero? n)
                       null
                       (cons (stx-car stx)
                             (loop (stx-cdr stx) (sub1 n)))))))

;; stx-list-drop/cx : stxish stx nat -> (values stxish stx)
(define (stx-list-drop/cx x cx n)
  (let loop ([x x] [cx cx] [n n])
    (if (zero? n)
        (values x
                (if (syntax? x) x cx))
        (loop (stx-cdr x)
              (if (syntax? x) x cx)
              (sub1 n)))))

;; check/force-syntax-list^depth : nat any id -> (listof^depth syntax)
;; Checks that value is (listof^depth syntax); forces promises.
;; Slow path for attribute-mapping code, assumes value is not syntax-list^depth? already.
(define (check/force-syntax-list^depth depth value0 source-id)
  (define (bad sub-depth sub-value)
    (attribute-not-syntax-error depth value0 source-id sub-depth sub-value))
  (define (loop depth value)
    (cond [(promise? value)
           (loop depth (force value))]
          [(zero? depth)
           (if (syntax? value) value (bad depth value))]
          [else (loop-list depth value)]))
  (define (loop-list depth value)
    (cond [(promise? value)
           (loop-list depth (force value))]
          [(pair? value)
           (let ([new-car (loop (sub1 depth) (car value))]
                 [new-cdr (loop-list depth (cdr value))])
             ;; Don't copy unless necessary
             (if (and (eq? new-car (car value))
                      (eq? new-cdr (cdr value)))
                 value
                 (cons new-car new-cdr)))]
          [(null? value)
           null]
          [else
           (bad depth value)]))
  (loop depth value0))

(define (attribute-not-syntax-error depth0 value0 source-id sub-depth sub-value)
  (raise-syntax-error #f
    (format (string-append "bad attribute value for syntax template"
                           "\n  attribute value: ~e"
                           "\n  expected for attribute: ~a"
                           "\n  sub-value: ~e"
                           "\n  expected for sub-value: ~a")
            value0
            (describe-depth depth0)
            sub-value
            (describe-depth sub-depth))
    source-id))

(define (describe-depth depth)
  (cond [(zero? depth) "syntax"]
        [else (format "list of depth ~s of syntax" depth)]))

;; syntax-list^depth? : nat any -> boolean
;; Returns true iff value is (listof^depth syntax).
(define (syntax-list^depth? depth value)
  (if (zero? depth)
      (syntax? value)
      (and (list? value)
           (for/and ([part (in-list value)])
             (syntax-list^depth? (sub1 depth) part)))))

;; datum->syntax/with-clause : any -> syntax
(define (datum->syntax/with-clause x)
  (cond [(syntax? x) x]
        [(2d-stx? x #:traverse-syntax? #f)
         (datum->syntax #f x #f)]
        [else
         (error 'datum->syntax/with-clause
                (string-append
                 "implicit conversion to 3D syntax\n"
                 " right-hand side of #:with clause or ~~parse pattern would be 3D syntax\n"
                 "  value: ~e")
                x)]))

;; check-literal* : id phase phase (listof phase) stx -> void
(define (check-literal* id used-phase mod-phase ok-phases/ct-rel ctx)
  (unless (or (memv (and used-phase (- used-phase mod-phase))
                    ok-phases/ct-rel)
              (identifier-binding id used-phase))
    (raise-syntax-error
     #f
     (format "literal is unbound in phase ~a (phase ~a relative to the enclosing module)"
             used-phase
             (and used-phase (- used-phase mod-phase)))
     ctx id)))

;; error/null-eh-match : -> (escapes)
(define (error/null-eh-match)
  (error 'syntax-parse "an ellipsis-head pattern matched an empty sequence"))

;; (begin-for-syntax/once expr/phase1 ...)
;; evaluates in pass 2 of module/intdefs expansion
(define-syntax (begin-for-syntax/once stx)
  (syntax-case stx ()
    [(bfs/o e ...)
     (cond [(list? (syntax-local-context))
            #`(define-values ()
                (begin (begin-for-syntax/once e ...)
                       (values)))]
           [else
            #'(let-syntax ([m (lambda _ (begin e ...) #'(void))])
                (m))])]))

;; == parse.rkt

(define (name->too-few/once name)
  (and name (format "missing required occurrence of ~a" name)))

(define (name->too-few name)
  (and name (format "too few occurrences of ~a" name)))

(define (name->too-many name)
  (and name (format "too many occurrences of ~a" name)))

;; == parse.rkt

;; normalize-context : Symbol Any Syntax -> (list Symbol/#f Syntax)
(define (normalize-context who ctx stx)
  (cond [(syntax? ctx)
         (list #f ctx)]
        [(symbol? ctx)
         (list ctx stx)]
        [(eq? ctx #f)
         (list #f stx)]
        [(and (list? ctx)
              (= (length ctx) 2)
              (or (symbol? (car ctx)) (eq? #f (car ctx)))
              (syntax? (cadr ctx)))
         ctx]
        [else (error who "bad #:context argument\n  expected: ~s\n  given: ~e"
                     '(or/c syntax? symbol? #f (list/c (or/c symbol? #f) syntax?))
                     ctx)]))

;; == parse.rkt

(lazy-require
 ["runtime-report.rkt"
  (call-current-failure-handler ctx fs)])

;; syntax-patterns-fail : (list Symbol/#f Syntax) -> (Listof (-> Any)) FailureSet -> escapes
(define ((syntax-patterns-fail ctx) undos fs)
  (unwind-to undos null)
  (call-current-failure-handler ctx fs))

;; == specialized ellipsis parser
;; returns (values 'ok attr-values) or (values 'fail failure)

(provide predicate-ellipsis-parser)

(define (predicate-ellipsis-parser x cx pr es pred? desc rl)
  (let ([elems (stx->list x)])
    (if (and elems (list? elems) (andmap pred? elems))
        (values 'ok elems)
        (let loop ([x x] [cx cx] [i 0])
          (cond [(syntax? x)
                 (loop (syntax-e x) x i)]
                [(pair? x)
                 (if (pred? (car x))
                     (loop (cdr x) cx (add1 i))
                     (let* ([pr (ps-add-cdr pr i)]
                            [pr (ps-add-car pr)]
                            [es (es-add-thing pr desc #t rl es)])
                       (values 'fail (failure pr es))))]
                [else ;; not null, because stx->list failed
                 (let ([pr (ps-add-cdr pr i)]
                       #|
                       ;; Don't extend es! That way we don't get spurious "expected ()"
                       ;; that *should* have been cancelled out by ineffable pair failures.
                       |#)
                   (values 'fail (failure pr es)))])))))

(provide illegal-cut-error)

(define (illegal-cut-error . _)
  (error 'syntax-parse "illegal use of cut"))

;; ----

(provide unwind-to
         maybe-add-state-undo
         current-state
         current-state-writable?
         state-cons!)

(define (unwind-to undos base)
  ;; PRE: undos = (list* proc/hash ... base)
  (unless (eq? undos base)
    (let ([top-undo (car undos)])
      (cond [(procedure? top-undo) (top-undo)]
            [(hash? top-undo) (current-state top-undo)]))
    (unwind-to (cdr undos) base)))

(define (maybe-add-state-undo init-state new-state undos)
  (if (eq? init-state new-state)
      undos
      (cons init-state undos)))

;; To make adding undos to rewind current-state simpler, only allow updates
;; in a few contexts:
;; - literals (handled automatically)
;; - in ~do/#:do blocks (sets current-state-writable? = #t)

(define current-state (make-parameter (hasheq)))
(define current-state-writable? (make-parameter #f))

(define (state-cons! key value)
  (define state (current-state))
  (current-state (hash-set state key (cons value (hash-ref state key null)))))
