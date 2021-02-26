#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    (begin)]
  [(version< (version) "6.90.0.29")
    (begin)]
  [else
    (my-include "../6-90-0-29/racket/collects/racket/private/template.rkt")])
