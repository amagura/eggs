;;; Add-in module to support using GNU readline from Chicken.
;
; (Readline is GPLed, so that makes this file GPLed too, because this
; file will only run if it's linked against readline.)
;
; Copyright (c) 2002 Tony Garnock-Jones
; Copyright (c) 2006 Heath Johns (paren bouncing and auto-completion code)
; Copyright (c) 2014 Alexej Magura (most of the history functions)
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;---------------------------------------------------------------------------
; csc -s readline.scm -L -lreadline -L -ltermcap
;
; To get csi to support line editing, install this library and put the
; following lines in your ~/.csirc:
;
;   (require 'readline)
;   (current-input-port (readline#make-readline-port "csi> "))
;
; If you also want to make the command history span sessions, add the
; following:
;
;   (readline#history-install-file-manager (string-append (or (getenv "HOME") ".") "/.csi.history"))
;
; By default this will save 1000 lines of history between sessions (it
; will prune the history file to 1000 lines at startup). For a
; different history size, pass the desired number of lines as the
; (optional) second argument to gnu-history-install-file-manager. If
; #f is passed in, no history-file-pruning will take place.
;
; To pass configuration options to readline (see the readline manual page for 
; details):
;
; (gnu-readline-parse-and-bind "set editing-mode vi")
;
;
; To change the "still waiting for input" prompt, just pass it as a second 
; argument to make-readline-port:
;
; (current-input-port (make-gnu-readline-port "csi> " ".... "))
;
;
; The neato line numbered display that's the csi default doesn't work, but 
; here's how to write a replacement:
;
;(repl-prompt 
;  (let ((history-count -1)) 
;	(lambda () 
;	  (set! history-count (+ 1 history-count)) 
;	  (sprintf "#;~A> " hist))))

(declare
 (usual-integrations))

; hack around missing export from `chicken' in 4.0.1:

(define ##readline#repl-prompt repl-prompt)

(module readline
  (export
    readline
    make-readline-port
    %signal-cleanup
    clear-history
    %read-history
    %write-history
    %append-history
    %truncate-history
    add-history
    add-history-time

    set-bounce-ms

    history-newlines
    history-list-length
    history-list

    history-get-entry

    history-current-entry
    history-previous-entry
    history-next-entry
    history-position
    search-history
    history-search
    search-history-forward
    history-search-forward
    search-history-backward
    history-search-backward

    use-legacy-bindings

    history-install-file-manager
    parse-and-bind
    completions)
  (import scheme chicken foreign ports data-structures)
  (use posix)

#>
 #include "readline-egg.c"
<#

;; Initialise (note the extra set of parens)
((foreign-lambda void "gnu_readline_init"))


;; Various C funcs

(define readline
  (foreign-safe-lambda c-string "gnu_readline_readline" c-string c-string))

(define %signal-cleanup
    (foreign-lambda void "gnu_readline_signal_cleanup"))

(define clear-history
  (foreign-lambda void "clear_history"))

;;; (gnu-readline-read-history <filename-or-false>) -> 0 for success, errno for failure
(define %read-history
  (foreign-lambda int "read_history" c-string))

;;; (gnu-readline-write-history <filename-or-false>) -> 0 for success, errno for failure
(define %write-history
  (foreign-lambda int "write_history" c-string))

(define %append-history
    (foreign-lambda int "gnu_readline_append_history" c-string))

(define set-bounce-ms
  (foreign-lambda* void ((int time))
                   "gnu_readline_bounce_ms = time;"))

;; (gnu-readline-truncate-history <filename-or-false> <numlines>) -> 0 succ, errno fail
(define %truncate-history
  (foreign-lambda int "history_truncate_file" c-string int))

(define add-history
  (foreign-lambda void "add_history" c-string))

(define add-history-time
  (foreign-lambda void "add_history_time" c-string))

(define history-newlines
    (foreign-lambda int "gnu_history_new_lines"))

(define (history-current-entry #!optional time)
  (if time
    (list
      (cons 'line
            ((foreign-lambda c-string "gnu_history_entry" int int) 0 0))
      (cons 'time
            ((foreign-lambda c-string "gnu_history_entry" int int) 0 1)))
    ((foreign-lambda c-string "gnu_history_entry" int int) 0 0)))

(define (history-previous-entry #!optional time)
  (if time
    (list
      (cons 'line
            ((foreign-lambda c-string "gnu_history_entry" int int) -1 0))
      (cons 'time
            ((foreign-lambda c-string "gnu_history_entry" int int) -1 1)))
    ((foreign-lambda c-string "gnu_history_entry" int int) -1 0)))

(define (history-next-entry #!optional time)
  (if time
    (list
      (cons 'line
            ((foreign-lambda c-string "gnu_history_entry" int int) 1 0))
      (cons 'time
            ((foreign-lambda c-string "gnu_history_entry" int int) 1 1)))
    ((foreign-lambda c-string "gnu_history_entry" int int) 1 0)))

(define (history-get-entry index #!optional time)
  (if time
    (list
      (cons 'line
            ((foreign-lambda c-string "gnu_history_get" int int) index 0))
      (cons 'time
            ((foreign-lambda c-string "gnu_history_get" int int) index 1)))
    ((foreign-lambda c-string "gnu_history_get" int int) index 0)))

;; (gnu-history-search "string" -1) -> search through previous entries
;; (gnu-history-search "string" 0+) -> search through subsequent entries.
;; returns match on succ, #f on fail.
;; xxx use `(get-keyword)' to access offset:, match:, and index:
;; xxx `index:' corresponds to the history-position within history_list of the match
(define (search-history string direction)
  (define offset
    ((foreign-lambda int "history_search" c-string int) string direction))
  (if (= offset -1)
    #f
    (list (cons 'match
                (list (cons 'line (history-current-entry))
                      (cons 'time (history-current-entry #t))
                      (cons 'offset offset)))
          (cons 'index (history-position)))))

(define history-search search-history)

(define (search-history-backward string)
  (search-history string -1))

(define history-search-backward search-history-backward)

(define (search-history-forward string)
  (search-history string 0))

(define history-search-forward search-history-forward)

(define history-list-length
  (foreign-lambda int "gnu_history_list_length"))

#| the underlying c-function works, but the scheme binding to said function is buggy.
 I just can't seem to get this function to do what I want it to do. :(
 it's supposed to take a string of history entries and transform it like so:

 "a\nb\nc\nd" -> ((1 . "a") (2 . "b") (3 . "c") (4 . "d"))

 |#
(define (history-list) ; FIXME ...err nevermind, I'm good for now.
  `(,@(string-split ((foreign-lambda c-string "gnu_history_list")) "\n")))

;; (gnu-history-position) -> current history position within history_list
;; (set! (history-position) pos) -> sets the current history position
(define (history-position)
    ((foreign-lambda int "where_history")))
(set! (setter history-position) (lambda (pos) ((foreign-lambda bool "history_set_pos" int) pos)))

;; Useful...
(define parse-and-bind
  (foreign-lambda int "rl_parse_and_bind" c-string))

; paren-bouncing support (comes with batteries included)
; XXX however, it doesn't work with LISP. 
;(parse-and-bind "set blink-matching-paren on")

;; get access to the quoting flag
(define-foreign-variable is-quoted? int "rl_completion_quote_character")
(define-foreign-variable filename-completion int "rl_filename_completion_desired")

;; Handler for the command history file
(define (history-install-file-manager filename . nlines)
  (define (hook param)
    (param (let ((next (param)))
	     (lambda args
	       ;(gnu-readline-write-history filename)
               (%append-history filename)
	       (apply next args)))))
  (if (pair? nlines)
      (set! nlines (car nlines))
      (set! nlines 1000))
  (if nlines
      (%truncate-history filename nlines))
  (%read-history filename)
  (hook exit-handler)
  (hook implicit-exit-handler))

;; Prompt2 is displayed when there are still open parens, this just makes a reasonable one
(define (make-prompt2 prompt)
  (let ((len (string-length prompt)))
   (case len
     ((0) "")
     ((1) ">")
     ((2) "> ")
     (else (conc (make-string (- len 2) #\-) "> ")))))


;; Creates a port that reads using readline
(define (make-readline-port #!optional prompt prompt2)
  (let ((buffer   "")
        (pos      0)
        (p1       prompt)
        (p2       prompt2) ;; removes the weird second prompt
        (handle   #f))
    (letrec ((char-ready?
               (lambda ()
                 (< pos (string-length buffer))))
             (get-next-char!
               (lambda ()
                 (cond ((not buffer)
                        #!eof)
                       ((char-ready?)
                        (let ((ch   (string-ref buffer pos)))
                         (set! pos (+ 1 pos))
                         ch))
                       (else
                         (set! pos 0)
                         (set! buffer
                           (let* ((prompt    (or prompt
                                                 ((##readline#repl-prompt))))
                                  (prompt2   (make-prompt2 prompt)))
                             (readline prompt prompt2)))
                         (if (string? buffer)
                           (set! buffer (string-append buffer "\n")))
                         (get-next-char!))))))
      (set! handle (lambda (s)
                     (print-call-chain)
                     (set! pos 0)
                     (set! buffer "")
                     ((foreign-lambda void
                                      "gnu_readline_signal_cleanup"))
                     (##sys#user-interrupt-hook)))
      (set-signal-handler! signal/int handle)
      (let ((p   (make-input-port
                   get-next-char!
                   char-ready?
                   (lambda ()
                     (set-signal-handler! signal/int #f)
                     'closed-gnu-readline-port))))
        (set-port-name! p "(gnu-readline)")
        p))))

#| Leagcy Bindings |#
(define (use-legacy-bindings)
  (and (map (lambda (a) (eval a))
            (do ((lst '((gnu-readline-readline
                         readline#readline)
                        (%gnu-readline-signal-cleanup
                          readline#%signal-cleanup)
                        (gnu-readline-clear-history
                          readline#clear-history)
                        (gnu-readline-read-history
                          readline#%read-history)
                        (gnu-readline-write-history
                          readline#%write-history)
                        (gnu-readline-append-history
                          readline#%append-history)
                        (gnu-readline-truncate-history
                          readline#%truncate-history)
                        (gnu-history-new-lines
                          readline#history-newlines)
                        (gnu-readline-parse-and-bind
                          readline#parse-and-bind)
                        (gnu-readline-set-bounce-ms
                          readline#set-bounce-ms)
                        (gnu-history-install-file-manager
                          readline#history-install-file-manager)
                        (make-gnu-readline-port
                          readline#make-readline-port)
                        (gnu-readline-completions
                          readline#completions))
                      (cdr lst))
                 (new-lst '()
                          (append new-lst
                                  (list `(define ,(caar lst) ,(cadar lst))))))
              ((null? lst) new-lst)))
       (void)))
;;;;;;;; Tab Completion ;;;;;;;;

;; Borrowed from the oblist egg
(define find-symbol-table (foreign-lambda c-pointer "C_find_symbol_table" c-string))
(define enum-symbols! (foreign-lambda scheme-object "C_enumerate_symbols" c-pointer scheme-object))

;; Globally defined enumeration state (callbacks can't be closures)
(define enum-funcs '())

;; Creates a list of closures that enumerate anything the user would want to type
(define (create-symbol-ef word)
	(let ((global-symbol-index (cons -1 '()))
		  (global-symbol-pointer (find-symbol-table ".")))
	  (lambda ()
		(let loop ()
		  (let ((symb (enum-symbols! global-symbol-pointer 
									 global-symbol-index)))
			(cond ((not symb)
				   "")
				  ((not (##sys#symbol-has-toplevel-binding? symb))
				   (loop))
				  (else
					(let ((str (##sys#symbol->qualified-string symb)))
					  ;; Possibly undo the mangling of keywords
					  (if (not (substring=? "###" str))
						str
						(string-append (substring str 3) ":"))))))))))
						
;; R5RS keywords (including some special forms not included in above)
(define (create-static-ef word)
	(let ((index -1)
		  (v-len (vector-length static-keywords)))
	  (lambda ()
		(set! index (+ 1 index))
		(if (not (>= index v-len))
		  (vector-ref static-keywords index)
		  ""))))
		  
;; Macros (thanks to Kon Lovett for suggesting ##sys#macro-environment)
(define (create-macro-ef word)
  (let ((index -1))
   (lambda ()
     (let ((macro-env (##sys#macro-environment)))
      (let loop ()
       (set! index (+ 1 index))
       (cond ((>= index (length macro-env))
              "")
             (else 
               (let ((ref (list-ref macro-env index)))
                (if (null? ref)
                  (loop)
                  (symbol->string (car ref)))))))))))

;; handling filename completion
(define (turn-on-filenames)
  (set! filename-completion 1))
  
(define (create-file-ef word)
  (turn-on-filenames)
  (let ((files (glob (string-append word "*"))))
    (lambda ()
      (if (null? files) ""
        (let ((current (car files)))
          (set! files (cdr files))
          current)))))
          
(define completions 
  (make-parameter 
    (list 
      (cons 'macros create-macro-ef)
      (cons 'statics create-static-ef) 
      (cons 'symbols create-symbol-ef))))

(define quoted-completions
  (make-parameter
    (list
      (cons 'files create-file-ef))))
      
;; This is the completion function called by readline
;; It's called repeatedly until it returns an empty string
;; (lambda'd to stop the compiler complaining about unused global var)
((lambda ()
   (define-external (gnu_readline_scm_complete (c-string word) (int len) (int state)) scheme-object
	 ;; If state is zero, init enumeration funcs.  Don't try to complete an empty string...
	 ;(print is-quoted?)
	 (when (zero? state)
	   (if (not (zero? len))
		 (set! enum-funcs (choose-completion-procs word))
		 ""))
	 ;; Call the enumeration funcs, discarding the ones that are done
	 (let loop ()
	   (if (null? enum-funcs)
		 ""
		 (let ((result ((car enum-funcs))))
		   (cond ((equal? result "")
				  (set! enum-funcs (cdr enum-funcs))
				  (loop))
				 ((substring=? word result 0 0 len)
				  result) ;; Return only ones that are a substring of the word typed
				 (else (loop)))))))))

;; This function simply chooses which completion type is appropriate
;; and then gets those procedures ready.
(define (choose-completion-procs word)
  (map
    (lambda (pair) ((cdr pair) word)) 
    (if (= 34 is-quoted?)
      (quoted-completions)
      (completions))))

;; Things that will always be there...
(define static-keywords (vector 
						; R5RS
						"abs" "acos" "and" "angle" "append" "apply" "asin" 
						"assoc" "assq" "assv" "atan" "begin" "boolean?" 
						"caar" "cadr" "call-with-current-continuation" 
						"call-with-input-file" "call-with-output-file" 
						"call-with-values" "car" "case" "cdddar" "cddddr" 
						"cdr" "ceiling" "char->integer" "char-alphabetic?" 
						"char-ci<=?" "char-ci<?" "char-ci=?" "char-ci>=?" 
						"char-ci>?" "char-downcase" "char-lower-case?" 
						"char-numeric?" "char-ready?" "char-upcase" 
						"char-upper-case?" "char-whitespace?" "char<=?" 
						"char<?" "char=?" "char>=?" "char>?" "char?" 
						"close-input-port" "close-output-port" "complex?" 
						"cond" "cons" "cos" "current-input-port" 
						"current-output-port" "define" "define-syntax" 
						"delay" "denominator" "display" "do" "dynamic-wind" 
						"else" "eof-object?" "eq?" "equal?" "eqv?" "eval" 
						"even?" "exact->inexact" "exact?" "exp" "expt" 
						"floor" "for-each" "force" "gcd" "if" "imag-part" 
						"inexact->exact" "inexact?" "input-port?" 
						"integer->char" "integer?" "interaction-environment" 
						"lambda" "lcm" "length" "let" "let*" "let-syntax" 
						"letrec" "letrec-syntax" "list" "list->string" 
						"list->vector" "list-ref" "list-tail" "list?" "load" 
						"log" "magnitude" "make-polar" "make-rectangular" 
						"make-string" "make-vector" "map" "max" "member" 
						"memq" "memv" "min" "modulo" "negative?" "newline" 
						"not" "null-environment" "null?" "number->string" 
						"number?" "numerator" "odd?" "open-input-file" 
						"open-output-file" "or" "output-port?" "pair?" 
						"peek-char" "port?" "positive?" "procedure?" 
						"quasiquote" "quote" "quotient" "rational?" 
						"rationalize" "read" "read-char" "real-part" 
						"real?" "remainder" "reverse" "round" 
						"scheme-report-environment" "set!" "set-car!" 
						"set-cdr!" "setcar" "sin" "sqrt" "string" 
						"string->list" "string->number" "string->symbol" 
						"string-append" "string-ci<=?" "string-ci<?" 
						"string-ci=?" "string-ci>=?" "string-ci>?" 
						"string-copy" "string-fill!" "string-length" 
						"string-ref" "string-set!" "string<=?" "string<?" 
						"string=?" "string>=?" "string>?" "string?" 
						"substring" "symbol->string" "symbol?" 
						"syntax-rules" "tan" #| "transcript-off" ; not implemented
						"transcript-on" |# "truncate" "values" "vector" 
						"vector->list" "vector-fill!" "vector-length" 
						"vector-ref" "vector-set!" "vector?" 
						"with-input-from-file" "with-output-to-file" 
						"write" "write-char" "zero?"
						; csi commands
						",?" ",p" ",d" ",du" ",dur" ",q" ",l" ",ln" ",r"
						",s" ",tr" ",utr" ",t" ",x" 
						))

)
