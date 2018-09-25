#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "7.0.0.20")
    (my-include "opt.rkt-6-90-0-29")]
  [else
    (my-include "opt.rkt-7-0-0-20")])
