;;;; readline.import.scm - GENERATED BY CHICKEN 4.9.0.1 -*- Scheme -*-

(eval '(import scheme chicken foreign ports data-structures posix))
(##sys#register-compiled-module
  'readline
  (list)
  '((readline . readline#readline)
    (make-readline-port . readline#make-readline-port)
    (%signal-cleanup . readline#%signal-cleanup)
    (clear-history . readline#clear-history)
    (%read-history . readline#%read-history)
    (%write-history . readline#%write-history)
    (%append-history . readline#%append-history)
    (%truncate-history . readline#%truncate-history)
    (add-history . readline#add-history)
    (add-history-time . readline#add-history-time)
    (set-bounce-ms . readline#set-bounce-ms)
    (history-newlines . readline#history-newlines)
    (history-list-length . readline#history-list-length)
    (history-list . readline#history-list)
    (history-get-entry . readline#history-get-entry)
    (history-current-entry . readline#history-current-entry)
    (history-previous-entry . readline#history-previous-entry)
    (history-next-entry . readline#history-next-entry)
    (history-position . readline#history-position)
    (search-history . readline#search-history)
    (history-search . readline#history-search)
    (search-history-forward . readline#search-history-forward)
    (history-search-forward . readline#history-search-forward)
    (search-history-backward . readline#search-history-backward)
    (history-search-backward . readline#history-search-backward)
    (use-legacy-bindings . readline#use-legacy-bindings)
    (history-install-file-manager . readline#history-install-file-manager)
    (parse-and-bind . readline#parse-and-bind)
    (completions . readline#completions))
  (list)
  (list))

;; END OF FILE
