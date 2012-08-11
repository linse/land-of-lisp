(ql:quickload :usocket) 


(defun decode-param (s)
   (labels ((f (lst)
               (when lst
                 (case (car lst)
                     (#\% (cons (code-char (parse-integer (coerce (list (cadr lst) (caddr lst)) 'string) :radix 16 :junk-allowed t))
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
  (let* ((url (subseq s
                      (+ 2 (position #\space s)) 
                      (position #\space s :from-end t)))
         (x (position #\? url)))
     (if x
         (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
         (cons url '()))))

(defun get-header (stream)
  (let* ((s (remove #\Return (read-line stream nil)))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))
 
(defun get-content-params (stream header)
  (let ((content (assoc 'content-length header)))
    (when content
      (parse-params (read-sequence (make-string (read content)) stream)))))

;;; In the serve function several things have changed but nothing that isn’t obvious now that it’s there.
;;; Figuring it out was pretty tricky so thanks to #lisp on freenode for the help.

(defun serve (request-handler)
  (let ((socket (usocket:socket-listen "127.0.0.1" 8080 :reuse-address t)))
    (unwind-protect
	 (loop (with-open-stream (stream (usocket:socket-stream (usocket:socket-accept socket)))
		 (let* ((url (parse-url (read-line stream nil)))
			(path (car url))
			(header (get-header stream))
			(params (append (cdr url)
					(get-content-params stream header)))
			(*standard-output* stream))
		   (funcall request-handler path header params))))
      (usocket:socket-close socket))))
 
(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<form>What is your name?<input name='name' /></form>")
            (format t "Nice to meet you, ~a!" (cdr name))))
      (princ "Sorry... I don't know that page.")))


