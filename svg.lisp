(defun print-tag (name alist closingp)
	(princ #\<)
	(when closingp
		(princ #\/))
	(princ (string-downcase name))
	(mapc (lambda (att)
					(format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
			alist)
	(princ #\>))
						
(defmacro tag (name atts &body body) 
	`(progn (print-tag ',name
										 (list ,@(mapcar (lambda (x) 
																				`(cons ',(car x) ,(cdr x)))
										 										(pairs att)))
										 nil) 
					 ,@body 
		(print-tag ',name nil t)))