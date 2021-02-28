#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(my-include "../../../" "/racket/collects/syntax/parse/experimental/private/substitute.rkt")
