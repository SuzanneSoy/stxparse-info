#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    (my-include "stxparse-info.scrbl-6-11")]
  [(version< (version) "6.90.0.29")
    (my-include "stxparse-info.scrbl-6-12")]
  [(version< (version) "7.3.0.1")
    (my-include "stxparse-info.scrbl-6-90-0-29")]
  [else
    (my-include "stxparse-info.scrbl-7-3-0-1")])
