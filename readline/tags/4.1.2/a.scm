    (##sys#extend-macro-environment
     'do*
     '()
     (##sys#er-transformer
      (lambda (form r c)
	(##sys#check-syntax 'do* form '(_ #((symbol _ . #(_)) 0) . #(_ 1)))
	(let* ((bindings (cadr form))
	       (test (caddr form))
	       (body (cdddr form))
	       (do*var (r 'doloop)))
	  `(let*
	       ,do*var
	     ,(##sys#map (lambda (b) (list (car b) (car (cdr b)))) bindings)
	     (##core#if ,(car test)
			,(let ((tbody (cdr test)))
			   (if (eq? tbody '())
			       '(##core#undefined)
			       `(##core#begin ,@tbody) ) )
			(##core#begin
			 ,(if (eq? body '())
			      '(##core#undefined)
			      `(##core#let () ,@body) )
			 (##core#app
			  ,do*var ,@(##sys#map (lambda (b)
						 (if (eq? (cdr (cdr b)) '())
						     (car b)
						     (car (cdr (cdr b))) ) )
					       bindings) ) ) ) ) ) ) ) )

    (block nil
	   (let ((var1 init1)
		 (var2 init2)
		 ...
		 (varn initn))
	     declarations
	     (loop (when end-test (return (progn . result)))
		   (tagbody . tagbody)
		   (psetq var1 step1
			  var2 step2
			  ...
			  varn stepn))))

    (expand-form '(do* ((a '(1 2 3) (cdr a))
			(b (mapcar (lambda (x) (1+ x)) a) (cdr b)))
		       ((= 1 1) (list 'a a 'b b))))
    (block nil
	   (let* ((a '(1 2 3)) (b (mapcar #'(lambda (x) (declare (system::source ((x) (1+ x)))) (1+ x)) a)))
	     (tagbody #:loop-5382 (if (= 1 1) (go #:end-5383)) (setq a (cdr a) b (cdr b)) (go #:loop-5382) #:end-5383 (return-from nil (list 'a a 'b b))))) ;
    ;t
