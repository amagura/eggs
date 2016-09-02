(require-extension test readline srfi-1 kvlists)
(test-begin "Readline variables")
(test-assert "readline#session is a kvlist"
	     (kvlist? readline#session))
(test-end)
(test-begin "Readline functions")
(readline#%add-history% "foobar")

(test-assert "history is consistent between calls"
             (string=? (last (readline#history-list)) "foobar"))

(test-assert "readline#history-position returns an integer"
             (integer? (readline#history-position)))

(test "readline#history-position's setter works"
      1
      (and (set! (readline#history-position) 1)
	   (readline#history-position)))

(readline#%add-history% "(print 1)")

(test "readline#eval-last-history-line works"
      (void)
      (readline#eval-last-history-line #t))


(test "readline#last-history-line is the last added line"
      "(print 1)"
      (readline#last-history-line #t #t))
(test-end)
(test-exit)
