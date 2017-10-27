(use readline posix utils irregex http-client uri-generic miscmacros)

(do ((eggs (string-split (call-with-input-pipe "chicken-status -list" read-all) (format "~%")) (cdr eggs)))
    ((null? eggs) '())
  (let* ((egg (string-split (irregex-replace/all '(or #\( #\) #\") (car eggs) " ")))
	 (name (car egg))
	 (local-version (cadr egg))
	 (todo '()))

    (pp
     (filter
      (lambda (y)
	(if (not (irregex-search '(or "error" "/guest") y))
	    (and (push! name todo) (pp todo) #t)
	    #f))
      (string-split (with-input-from-request
		     (string-append
		      "http://chicken.kitten-technologies.co.uk/henrietta.cgi?name="
		      (uri-encode-string name)
		      "&listversions=1") #f read-all) (format "~%"))))))

(define (needs-update egg-name #!optional url)
  (let ((.url.
	 (if url
	     url
	     "http://chicken.kitten-technologies.co.uk/henrietta.cgi?name=")))
    (pp .url.)
    (pp egg-name)
    (-(call-with-input-pipe "chicken-status -list")
    (last
     (versions#version-sort
      (string-split
       (with-input-from-request
	(string-append
	 .url.
	 (uri-encode-string egg-name)
	 "&listversions=1") #f read-all)
       (format "~%")))))))

(define (local egg)
  (irregex-replace (format "~%")
		   (last (string-split (call-with-input-pipe
					(string-append "chicken-status " egg)
					read-all)
				       " "))))

(define (remote egg #!optional url)
  (let ((.url. (if url url "http://chicken.kitten-technologies.co.uk/henrietta.cgi?name=")))
    (versions#version-sort (string-split (with-input-from-request (string-append .url. (uri-encode-string egg)
										 "&listversions=1")
								  #f read-all)
					 (format "~%")))))

(define (update-available? egg #!optional url)
  (versions#version-newer? (last (remote egg)) (local egg)))
