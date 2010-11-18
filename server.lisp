(defun http-char (c1 c2 &optional (default #\Space))
	(let ((code (parse-integer
							(coerce (list c1 c2) 'string)
							:radix 16
							:junk-allowed t)))
	  (if code
				(code-char code)
				default)))
				
(defun decode-param (s)
	(labels ((f (lst)
							(when lst
										(case (car lst)
												(#\% (cons (http-char (cadr lst) (caddr lst))
																		(f (cdddr lst))))
												(#\+ (cons #\space (f (cdr lst))))
												(otherwise (cons (car lst) (f (cdr lst))))))))
	  (coerce (f (coerce s 'list)) 'string)))
	
(defun parse-params (s)
	(let* ((i1 (position #\= s))
					(i2 (position #\& s)))
		(cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
													(decode-param (subseq s (1+ i1) i2)))
										(and i2 (parse-params (subseq s (1+ i2))))))
					((equal s "") nil)
					(t s))))
					
(defun parse-url (s)
	(let* ((url (subseq s (+ 2 (position #\space s))
												(position #\space s :from-end t)))
				  (x (position #\? url)))
	  (if x 
				(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
				(cons url '()))))
				
(defun get-header (stream) (let* ((s (read-line stream))
(h (let ((i (position #\: s))) (when i
(cons (intern (string-upcase (subseq s 0 i))) (subseq s (+ i 2)))))))
(when h (cons h (get-header stream)))))