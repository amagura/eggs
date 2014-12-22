
(foreign-declare #<<EOF
                 
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

EOF
)

(define readline (foreign-lambda c-string readline c-string))
(define add-history (foreign-lambda void add_history c-string))
(define rl-refresh-line (foreign-lambda int rl_refresh_line int int))

(add-history "test")
(add-history (readline "> "))
(readline "> ")
(rl-refresh-line 0 0)
(newline)
