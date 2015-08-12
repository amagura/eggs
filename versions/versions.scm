;;;; egg:       versions
;;;; file:      versions.scm
;;;; author:    elf <elf@ephemeral.net>
;;;; author:    david krentzlin
;;;; date:      07 Apr 2008
;;;; licence:   BSD (see LICENCE)
;;;; dialect:   r5rs
;;;; requires:  srfi-1, srfi-13, srfi-23
;;;; purpose:   version parsing, creation, and ordering procedures




;;; chicken library loading



(module versions
(make-version
 version?
 version:label
 version:major
 version:major-set!
 version:minor
 version:micro
 version:patch
 version:extra
 string->version
 version->string
 version-compare
 version=?
 version<?
 version<=?
 version>?
 version>=?
 version-newer?
 version-older?
 version-exact?
 version-sort
 update-version
 bump:major
 bump:minor
 bump:micro
 bump:patch
 bump
 bump:major!
 bump:minor!
 bump:micro!
 bump:patch!
 bump!
 local
 remote
 update-available?

 )

(import chicken scheme)
(require-library srfi-1 srfi-13)
(import srfi-1 srfi-13 data-structures irregex posix utils)
(import (only extras fprintf))
(use http-client uri-generic)


;;; version type

(define-record-type version
    (version-new label major minor micro patch extra)
    version?
    (label    version:label)
    (major    version:major version:major-set!)
    (minor    version:minor version:minor-set!)
    (micro    version:micro version:micro-set!)
    (patch    version:patch version:patch-set!)
    (extra    version:extra))

(define-record-printer (version v out)
  (fprintf out "#<version ~A>" (version->string v)))

;;; conversion methods


;; (##ver#char->num digit prev)
;; converts a digit char to its numeric 
(define-inline (##ver#char->num d p)
    (fx+ (fx* 10 p) (fx- (char->integer d) 48)))

;; (##ver#make-version prefix version-nums suffix)
;; creates a version object, if version-nums of at least length 2
(define-inline (##ver#make-version p a s)
    (let ((a   (if (car a)
                   (reverse a)
                   (begin
                       (set! s (string-append "." (or s "")))
                       (reverse (cdr a)))))
          (p   (if (string-null? p)
                   #f
                   p)))
        (case (length a)
            ((0 1)
                #f)
            ((2)
                (version-new p (car a) (cadr a) #f #f s))
            ((3)
                (version-new p (car a) (cadr a) (caddr a) #f s))
            (else
                (version-new p (car a) (cadr a) (caddr a) (cdddr a) s)))))

;; (string->version version-string)
;; converts a string into a version object (or #f on failure)
(define (string->version str)
    (##sys#check-string str 'string->version)
    (or (fx> (##sys#size str) 0)
        (##sys#signal-hook #:type-error 'string->version
                           "argument is not a non-null string" str))
    (let loop ((l   (string->list str))
               (p   "")
               (a   (list #f)))
        (if (null? l)
            (##ver#make-version p a #f)
            (case (car l)
                ((#\_ #\-)
                    (if (fx= 1 (length a))
                        (if (car a)
                            (loop (cdr l)
                                  (string-append p
                                                 (number->string (car l))
                                                 (string (car l)))
                                  (list #f))
                            (loop (cdr l)
                                  (string-append p
                                                 (string (car l)))
                                  a))
                        (if (car a)
                            (##ver#make-version p a (list->string l))
                            #f)))
                ((#\.)
                    (if (car a)
                        (loop (cdr l)
                              p
                              (cons #f a))
                        (if (fx= 1 (length a))
                            (loop (cdr l)
                                  (string-append p ".")
                                  a)
                            #f)))
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (if (car a)
                        (loop (cdr l)
                              p
                              (cons (##ver#char->num (car l) (car a)) (cdr a)))
                        (loop (cdr l)
                              p
                              (cons (##ver#char->num (car l) 0) (cdr a)))))
                (else
                    (if (fx= 1 (length a))
                        (if (car a)
                            (loop (cdr l)
                                  (string-append p
                                                 (number->string (car a))
                                                 (string (car l)))
                                  (list #f))
                            (loop (cdr l)
                                  (string-append p
                                                 (string (car l)))
                                  a))
                        (if (car a)
                            (##ver#make-version p a (list->string l))
                            (if (fx> (length a) 2)
                                (##ver#make-version p a (list->string l))
                                (loop (cdr l)
                                      (string-append p
                                                     (number->string (cadr a))
                                                     (string #\. (car l)))
                                      (list #f))))))))))

;; (version->string version)
;; creates a version-string from a version
(define (version->string ver)
    (or (version? ver)
        (##sys#signal-hook #:type-error 'version->string
                           "argument is not a version record"
                           ver))
    (string-append (or (version:label ver)
                       "")
                   (number->string (version:major ver))
                   "."
                   (number->string (version:minor ver))
                   (if (version:micro ver)
                       (string-append "."
                                      (number->string (version:micro ver)))
                       "")
                   (if (version:patch ver)
                       (string-join (map number->string (version:patch ver))
                                    "."
                                    'prefix)
                       "")
                   (or (version:extra ver)
                       "")))



;;; typecheck methods


;; (##ver#test-exact val loc)
;; ensures val is a nonnegative exact integer
(define-inline (##ver#test-exact val loc)
    (or (and (integer? val)
             (exact? val)
             (fx>= val 0))
        (##sys#signal-hook #:type-error loc
                           "argument is not an exact non-negative integer"
                           val)))

;; (##ver#check-string val loc)
;; ensures val is either #f or a non-null string
(define-inline (##ver#check-string val loc)
    (and val
         (##sys#check-string val loc)
         (or (fx> (##sys#size val) 0)
             (##sys#signal-hook #:type-error loc 
                                "argument is not #f or a non-null string"
                                val))))

;; (##ver#check-exact val loc)
;; ensures val is either #f or an exact integer
(define-inline (##ver#check-exact val loc)
    (and val
         (##ver#test-exact val loc)))

;; (##ver#check-version val loc)
;; ensures val is either a version obj or a version string (returns version obj)
(define-inline (##ver#check-version val loc)
    (if (version? val)
        val
        (or (and (string? val)
                 (string->version val))
            (##sys#signal-hook #:type-error loc
                               "argument is not a version or version-string"
                               val))))



;;; exported version record creator


;; (make-version major minor #!key label micro patch extra)
;; creates a version object with most fields optional
(define (make-version maj mnr #!key (label #f) (micro #f) (patch #f) (extra #f))
  (##sys#check-exact maj 'make-version)
  (##sys#check-exact mnr 'make-version)
  (##ver#check-exact micro 'make-version)
  (##ver#check-string label 'make-version)
  (##ver#check-string extra 'make-version)
  
  (and patch
       (cond ((list? patch)
              (if (null? patch)
                  (set! patch #f)
                  (every (cut ##ver#test-exact <> 'make-version) patch)))
             ((vector? patch)
              (set! patch (vector->list patch))
              (if (null? patch)
                  (set! patch #f)
                  (every (cut ##ver#test-exact <> 'make-version) patch)))
             ((##ver#test-exact patch 'make-version)
              (set! patch (list patch)))
             (else
              (##sys#signal-hook #:type-error 'make-version
                                 "argument is not a list, exact, or #f"
                                 patch))))
  
  (version-new label maj mnr micro patch extra))



;;; version comparators


;; (##ver#version-compare v1 v2)
;; compare two version objects
;; -1: v1 < v2   0: v1 = v2   1: v1 > v2
(define-inline (##ver#version-compare v1 v2)
    (call-with-current-continuation
        (lambda (k)
            (if (version:label v1)
                (if (version:label v2)
                    (if (string<? (version:label v1) (version:label v2))
                        (k -1)
                        (and (string>? (version:label v1) (version:label v2))
                             (k 1)))
                    (k 1))
                (and (version:label v2)
                     (k -1)))
            (if (fx< (version:major v1) (version:major v2))
                (k -1)
                (and (fx> (version:major v1) (version:major v2))
                     (k 1)))
            (if (fx< (version:minor v1) (version:minor v2))
                (k -1)
                (and (fx> (version:minor v1) (version:minor v2))
                     (k 1)))
            (if (version:micro v1)
                (if (version:micro v2)
                    (if (fx< (version:micro v1) (version:micro v2))
                        (k -1)
                        (and (fx> (version:micro v1) (version:micro v2))
                             (k 1)))
                    (k 1))
                (and (version:micro v2)
                     (k -1)))
            (if (version:patch v1)
                (if (version:patch v2)
                    (let loop ((l1   (version:patch v1))
                               (l2   (version:patch v2)))
                        (cond ((null? l1)
                                  (or (null? l2)
                                      (k -1)))
                              ((null? l2)
                                  (k 1))
                              ((fx= (car l1) (car l2))
                                  (loop (cdr l1) (cdr l2)))
                              ((fx< (car l1) (car l2))
                                  (k -1))
                              (else
                                  (k 1))))
                    (k 1))
                (and (version:patch v2)
                     (k -1)))
            (if (version:extra v1)
                (if (version:extra v2)
                    (if (string<? (version:extra v1) (version:extra v2))
                        (k -1)
                        (and (string>? (version:extra v1) (version:extra v2))
                             (k 1)))
                    (k 1))
                (and (version:extra v2)
                     (k -1)))
            0)))

;; (version-compare version1 version2)
;; compares version1 and version2 (version strings or versions)
;; -1: v1 < v2   0: v1 = v2   1: v1 > v2
(define (version-compare v1 v2)
    (##ver#version-compare (##ver#check-version v1 'version-compare)
                           (##ver#check-version v2 'version-compare)))

;; (version=? version1 version2)
;; compares version1 and version2 (version strings or versions): #t if v1 = v2
(define (version=? v1 v2)
    (fx= 0 (##ver#version-compare (##ver#check-version v1 'version=?)
                                  (##ver#check-version v2 'version=?))))

;; (version<? version1 version2)
;; compares version1 and version2 (version strings or versions): #t if v1 < v2
(define (version<? v1 v2)
    (fx= -1 (##ver#version-compare (##ver#check-version v1 'version<?)
                                   (##ver#check-version v2 'version<?))))

;; (version<=? version1 version2)
;; compares version1 and version2 (version strings or versions): #t if v1 <= v2
(define (version<=? v1 v2)
    (fx> 1 (##ver#version-compare (##ver#check-version v1 'version<=?)
                                  (##ver#check-version v2 'version<=?))))

;; (version>=? version1 version2)
;; compares version1 and version2 (version strings or versions): #t if v1 >= v2
(define (version>=? v1 v2)
    (fx< -1 (##ver#version-compare (##ver#check-version v1 'version>=?)
                                   (##ver#check-version v2 'version>=?))))

;; (version>? version1 version2)
;; compares version1 and version2 (version strings or versions): #t if v1 > v2
(define (version>? v1 v2)
    (fx= 1 (##ver#version-compare (##ver#check-version v1 'version>?)
                                  (##ver#check-version v2 'version>?))))

;; (version-newer? version1 version2)
;; (version-older? version1 version2)
;; (version-exact? version1 version2)
;; alternate names for version>?, version<? and version=?, respectively
(define version-newer? version>?)
(define version-older? version<?)
(define version-exact? version=?)



;;; sorting procedures


;; (##ver#sort-inc v1 v2)
;; helper procedure for sorting in ascending order
(define (##ver#sort-inc v1 v2)
    (fx= -1 (##ver#version-compare (cdr v1) (cdr v2))))

;; (##ver#sort-dec v1 v2)
;; helper procedure for sorting in descending order
(define (##ver#sort-dec v1 v2)
    (fx= 1 (##ver#version-compare (cdr v1) (cdr v2))))

;; (version-sort version-list #!optional ascending?)
;; sorts version-list (versions or version-strings)
;; default is ascending order
(define (version-sort vl #!optional (asc? #t))
    (map
        car
        (sort
            (map
                (lambda (d)
                    (cons d (##ver#check-version d 'version-sort)))
                vl)
            (if asc?
                ##ver#sort-inc
                ##ver#sort-dec))))


;; Bumping interface

;; functional interface
(define (update-version version #!key (major #f) (minor #f) (micro #f) (patch #f))
  (make-version
   (or major (version:major version))
   (or minor (version:minor version))
   label: (version:label version)
   micro: (or micro (version:micro version))
   patch: (or patch (version:patch version))
   extra: (version:extra version)))

(define ((make-version-bumber field getter loc) version #!key (to #f))
  (let* ((version (##ver#check-version version loc))
         (value   (getter version))
         (new-value (if to to (if value (+ 1 value) 1))))
    (update-version version field new-value)))

(define bump:major (make-version-bumber major: version:major 'bump:major))
(define bump:minor (make-version-bumber minor: version:minor 'bump:minor))
(define bump:micro (make-version-bumber micro: version:micro 'bump:micro))

(define (bump:patch version #!key (to #f))
  (let* ((version (##ver#check-version version 'bump:patch))
         (patch (version:patch version)))
    (cond
     (to
      (update-version version patch: to))
     ((list? patch)
      (let* ((last-version (last patch))
             (new-version (reverse (cons (+ 1  last-version) (cdr (reverse patch))))))
        (update-version version patch: new-version)))
     (else
      (update-version version patch: (list 1))))))

(define (bump version)
  (let ((version (##ver#check-version version 'bump)))
    (cond
     ((version:patch version) (bump:patch version))
     ((version:micro version) (bump:micro version))
     ((version:minor version) (bump:minor version)))))

;; side-efecting interface
(define ((make-version-bumper! getter setter loc) version #!key (to #f))
  (let* ((version (##ver#check-version version loc))
         (value (getter version))
         (new-value (if to to (if value (+ 1 value) 1))))
    (setter version new-value)
    version))

(define bump:major! (make-version-bumper! version:major version:major-set! 'bump:major!))
(define bump:minor! (make-version-bumper! version:minor version:minor-set! 'bump:minor!))
(define bump:micro! (make-version-bumper! version:micro version:micro-set! 'bump:micro!))

(define (bump:patch! version #!key (to #f))
  (let* ((version (##ver#check-version version 'bump:patch!))
         (patch (version:patch version)))
    (cond
     (to
      (version:patch-set! version to))
     ((list? patch)
      (let* ((last-version (last patch))
             (new-version (reverse (cons (+ 1  last-version) (cdr (reverse patch))))))
        (version:patch-set! version new-version)))
     (else 
      (version:patch-set! version (list 1)))))
  version)

(define (bump! version)
  (let ((version (##ver#check-version version 'bump!)))
    (cond
     ((version:patch version) (bump:patch! version))
     ((version:micro version) (bump:micro! version))
     ((version:minor version) (bump:minor! version)))))

(define (local egg)
  (irregex-replace
   (format "~%")
   (last
    (string-split
     (call-with-input-pipe
      (string-append "chicken-status " egg)
      read-all)))))

(define (remote egg #!optional url)
  (let ((.url.
	 (if url
	     url
	     "http://chicken.kitten-technologies.co.uk/henrietta.cgi?name=")))
    (version-sort
     (string-split
      (with-input-from-request
       (string-append
	.url.
	(uri-encode-string egg)
	"&listversions=1")
       #f read-all)
      (format "~%")))))

(define (update-available? egg #!optional url)
  (version-newer? (last (remote egg)) (local egg)))


)

