#!/bin/bash
for i in `find -name '*-6-11'`; do echo "$i"; j="$i"; i="$(basename "$i")"; cat > "${j%-6-11}" <<EOF
$(head -n 1 "$j")
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    (my-include "$i")]
  [else
    $(if test -e "${j%-6-11}-6-12"; then echo "(my-include \"${i%-6-11}-6-12\")"; else echo "(begin)"; fi)])
EOF
done