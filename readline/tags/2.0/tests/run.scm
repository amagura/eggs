(require-extension test readline srfi-1)
(test-begin "readline-history")
(test-assert "(readline#add-history STRING) == (void)"
             (equal? (void) (readline#add-history "(last (readline#history-list))")))

(test-assert "(readline#history-list) == LIST"
             (list? (readline#history-list)))

(test-assert "(last (readline#history-list)) == \"(last (readline#history-list))\""
             (string=? (last (readline#history-list)) (->string '(last (readline#history-list)))))

(test-assert "(readline#history-position) == INTEGER"
             (integer? (readline#history-position)))

(test "(set! (readline#history-position) -1) == #f"
      #f
      (set! (readline#history-position) -1))

(test "(readline#history-get-entry -1) == #f"
      #f
      (history-get-entry -1))

(test-error "(readline#add-history) == (error)"
            (readline#add-history))

(test-assert "(readline#history-current-entry) == (last (readline#history-list))"
             (string=? (readline#history-current-entry) (last (readline#history-list))))

(readline#add-history "hello")
(readline#add-history "blue")

(test-assert "(readline#search-history-backward \"hello\") == '((match . (line . STRING) (time . (or STRING #f))) (index . INTEGER))"
             (let* ((result (readline#history-search-forward "hello"))
                   (match (alist-ref 'match result)))
               (string? (alist-ref 'line match))
               (or (string? (alist-ref 'time match)) (boolean? (alist-ref 'time match)))
               (integer? (alist-ref 'index result))))

#|
;; needs epoch, I think.
(test "(readline#add-history-time 

(test-assert "(readline#search-history-backward \"hello\") == '(match: (line: STRING time: STRING or #f) index: INTEGER)"
             (let* ((result (readline#history-search-backward "hello"))
                   (match (get-keyword match: result)))
               (string? (get-keyword line: match))
               (or (string? (get-keyword time: match)) (boolean? (get-keyword time: match)))
               (integer? (get-keyword index: result))))
|#
(test-end)
(test-exit)
