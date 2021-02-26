#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    (my-include "../6-11/racket/collects/racket/private/stxloc.rkt")]
  [(version< (version) "6.90.0.29")
    ;; TODO: this seems like a bug, it should be 6-12
    (my-include "../6-11/racket/collects/racket/private/stxloc.rkt")]
  [else
    (my-include "../6-90-0-29/racket/collects/racket/private/stxloc.rkt")])
