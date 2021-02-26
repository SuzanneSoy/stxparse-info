#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    (my-include "../6-11/racket/collects/racket/private/with-stx.rkt")]
  [(version< (version) "6.90.0.29")
    ;; TODO: this seems like a bug, should be 6-12
    (my-include "../6-11/racket/collects/racket/private/with-stx.rkt")]
  [else
    (my-include "../6-90-0-29/racket/collects/racket/private/with-stx.rkt")])
