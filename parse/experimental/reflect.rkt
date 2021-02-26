#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    (my-include "../../6-11/racket/collects/syntax/parse/experimental/reflect.rkt")]
  [(version< (version) "6.90.0.29")
    (my-include "../../6-12/racket/collects/syntax/parse/experimental/reflect.rkt")]
  [(version< (version) "7.0.0.20")
    (my-include "../../6-90-0-29/racket/collects/syntax/parse/experimental/reflect.rkt")]
  [else
    (my-include "../../7-0-0-20/racket/collects/syntax/parse/experimental/reflect.rkt")])
