#!/bin/bash
for i in `find -name '*-6-11' -or -name '*-6-12' -or -name '*-6-90.0.29'`; do
  echo "${i%-6-*}"
done | sort -u | while read pathmain; do
  echo "$pathmain"
  main="$(basename "$pathmain")"

  if   test -e "${pathmain}-6-11"; then              eleven="(my-include \"${main}-6-11\")";
  else                                               eleven="(begin)"; fi

  if   test -e "${pathmain}-6-12"; then              twelve="(my-include \"${main}-6-12\")";
  elif test -e "${pathmain}-6-12.deleted"; then      twelve="(begin)";
  else                                               twelve="$eleven"; fi

  if   test -e "${pathmain}-6-90-0-29"; then         twentynine="(my-include \"${main}-6-90-0-29\")";
  elif test -e "${pathmain}-6-90-0-29.deleted"; then twelve="(begin)";
  else                                               twentynine="$twelve"; fi

  cat > "$pathmain" <<EOF
#lang racket/base
(#%require version-case
           (for-syntax (only racket/base version)
                       (only racket/base #%app #%datum))
           stxparse-info/my-include)
(version-case
  [(version< (version) "6.11.0.900")
    $eleven]
  [(version< (version) "6.90.0.29")
    $twelve]
  [else
    $twentynine])
EOF
done
