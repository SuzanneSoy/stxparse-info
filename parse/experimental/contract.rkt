#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "7.3.0.1")
    (my-include "../../7-0-0-20/racket/collects/syntax/parse/experimental/contract.rkt")]
  [else
    (my-include "../../7-3-0-1/racket/collects/syntax/parse/experimental/contract.rkt")])
