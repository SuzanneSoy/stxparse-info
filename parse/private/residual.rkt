#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    (my-include "residual.rkt-6-11")]
  [(version< (version) "6.90.0.29")
    (my-include "residual.rkt-6-12")]
  [(version< (version) "7.0.0.20")
    (my-include "residual.rkt-6-90-0-29")]
  [else
    (my-include "residual.rkt-7-0-0-20")])
