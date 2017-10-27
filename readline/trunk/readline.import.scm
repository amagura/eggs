;;;; readline.import.scm - GENERATED BY CHICKEN 4.12.0 -*- Scheme -*-

(eval '(import scheme chicken foreign ports data-structures posix irregex))
(##sys#register-compiled-module
  'readline
  (list)
  '((*session* . readline#*session*)
    (*version* . readline#*version*)
    (readline . readline#readline)
    (make-readline-port . readline#make-readline-port)
    (clear-history . readline#clear-history)
    (%signal-cleanup . readline#%signal-cleanup)
    (read-history . readline#read-history)
    (write-history . readline#write-history)
    (append-history . readline#append-history)
    (truncate-history . readline#truncate-history)
    (%add-history% . readline#%add-history%)
    (%remove-history% . readline#%remove-history%)
    (set-bounce-ms . readline#set-bounce-ms)
    (history-newlines . readline#history-newlines)
    (history-list . readline#history-list)
    (history-position . readline#history-position)
    (search-history . readline#search-history)
    (search-history-starts-with . readline#search-history-starts-with)
    (search-history-from-position . readline#search-history-from-position)
    (search-history-forward . readline#search-history-forward)
    (search-history-backward . readline#search-history-backward)
    (search-history-starts-with-forward
      .
      readline#search-history-starts-with-forward)
    (search-history-starts-with-backward
      .
      readline#search-history-starts-with-backward)
    (current-history-line . readline#current-history-line)
    (last-history-line . readline#last-history-line)
    (eval-last-history-line . readline#eval-last-history-line)
    (install-history-file . readline#install-history-file)
    (parse-and-bind . readline#parse-and-bind)
    (completions . readline#completions)
    (variables . readline#variables))
  (list)
  (list))

;; END OF FILE
