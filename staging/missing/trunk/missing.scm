#|
  /- filename -/
  missing.scm

  /- copyright -/
  Copyright 2015 Alexej Magura
  All Rights Reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
|#

(module missing
    (
     ;;; math
     1+
     1-
     ;;; logic
     /=
     !*
     false?
     true?
     false?*
     true?*
     ;;; trees
     do*
     depth
     ;;; assignment
     setq
     ;;; eggs
     egg-versions
     local-egg-version
     remote-egg-versions

     ;;; variables (actual variables; not functions or macros)
     settings
     )

(import chicken scheme data-structures
	irregex posix utils srfi-1 extras
	csi)
(use http-client uri-generic versions)

#|/////////////////////////////////|#
;;;; Private Functions
#|/////////////////////////////////|#
(define-inline (setkv! kvlst key value)
  (let ((kv (member key kvlst)))
    (if kv
	(set-car! (cdr kv) value))))

(define-inline (getkv kvlst key #!optional default)
  (let ((kv (member key kvlst)))
    (if kv
	(cadr kv)
	default)))
#|/////////////////////////////////|#
;;;; Public Variables
#|/////////////////////////////////|#
(define settings '(#:egg-actions-need-sudo #t))
#|/////////////////////////////////|#
;;;; Public Functions
#|/////////////////////////////////|#

;;; --- math ---

(define (1+ n) (+ 1 n))
(define (1- n) (- 1 n))

;;; --- logic (tests) ---

(define (/= exp)
  (not (= exp)))

(define (!* fn #!rest ...)
  (not (eval `(,fn ,@...))))

(define (false? exp)
  (not (and exp #t)))

(define (true? exp)
  (and exp #t))

(define (false?* exp)
  (equal? #f exp))

(define (true?* exp)
  (equal? #t exp))

(define (non-zero? exp)
  (if (not (or (number? exp)
	       (integer? exp)))
      #t
      (not (zero? exp))))

;;; --- trees ---

(define (depth tree)
  (if (not-pair? tree) 0
      (1+ (apply max (map depth tree)))))

(define-syntax do*
  (syntax-rules ()
    ((_ ((var init step) ...)
	(test ... result)
	body ...)
     (let* ((var init) ...)
       (let loop ()
         (cond (test ... result)
               (else body ...
                     (set! var step) ...
                     (loop))))))
    ((_ )
     (error "The macro do* may not be called with 0 arguments: (do*)"))))

;;; --- assignment ---

(define-syntax setq
  (syntax-rules ()
    ((_ a b)
     (begin (set! a b)
	    b))
    ((_ a b c ...)
     (begin (set! a b)
	    (setq c ...)))
    ((_ a)
     (error (string-append
	     "setq: odd number of arguments: ("
	     (->string (quote a))
	     ")")))
    ((_ )
     #f)))

;;; --- eggs ---

(define (local-egg-version egg)
  (irregex-replace
   (format "~%")
   (last
    (string-split
     (call-with-input-pipe
      (string-append "chicken-status " egg)
      read-all)))))

(define (remote-egg-versions egg #!optional url)
  (let ((.url.
	 (if url
	     url
	     "http://chicken.kitten-technologies.co.uk/henrietta.cgi?name=")))
    (versions#version-sort
     (string-split
      (http-client#with-input-from-request
       (string-append
	.url.
	(uri-encode-string egg)
	"&listversions=1")
       #f read-all)
      (format "~%")))))

(define (update-available? egg #!optional url)
  (versions#version-newer? (last (remote-egg-versions egg)) (local-egg-version egg)))

;; find the versions for the given egg; `remote' defaults to #f
(define (egg-versions name remote #!optional url)
  (if (and (true? remote) ;; if remote, then
	   (non-zero? remote))
      (remote-egg-versions name url)
      (local-egg-version name)))

)


(toplevel-command '+install (lambda ()
			  (system
			   (string-append
			    "chicken-install"
			    (if (getkv missing#settings #:egg-actions-need-sudo)
				" -s "
				" ")
			    (read-line))))
		  ",+install         Install an egg")

(toplevel-command '-uninstall (lambda ()
			  (system
			   (string-append
			    "chicken-uninstall"
			    (if (getkv missing#settings #:egg-actions-need-sudo)
				" -s "
				" ")
			    (read-line))))
		  ",-uninstall       Uninstall an egg")

(toplevel-command '+update (lambda ()
			     (let* ((eggs (string-split (call-with-input-pipe
							"chicken-status -eggs"
							read-all)
						       (format "~%")))
				   (toup (filter missing#update-available? eggs))
				   )
			       (if (not (yes-or-no?
					 (string-append
					  "About to update "
					  (number->string (length toup))
					  " eggs: proceed?")))
				   (void)
				   (do ((left toup (cdr toup)))
				       ((null? left))
				     (system (string-append
					      "chicken-install"
					      (if (getkv missing#settings #:egg-actions-need-sudo)
						  " -s "
						  " ")
					      (car toup)))))))
		  ",+update          Update installed eggs")

#;(toplevel-command 'egg? (lambda ()
			  (print (call-with-input-pipe
				  (string-append
				   "chicken-install -list")))))
