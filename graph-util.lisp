(defparameter *nodes* '(
    (living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden. there is a well in front of you.))
    (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defun describe-location (location nodes) (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door) (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room) 
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
            (eq (cadr (assoc obj obj-locs)) loc))) (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc) (labels ((describe-obj (obj)
  `(you see a ,obj on the floor.)))
  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
  (describe-paths *location* *edges*)
  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
 (let ((next (find direction
                   (cdr (assoc *location* *edges*))
                   :key #'cadr)))
 (if next
     (progn (setf *location* (car next))
            (look))
     '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*)) 
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun say-hello ()
(princ "Please type your name: ") (let ((name (read-line)))
         (princ "Nice to meet you, ")
         (princ name)))

(defun game-repl ()
   (let ((cmd (game-read)))
       (unless (eq (car cmd) 'quit)
           (game-print (game-eval cmd))
           (game-repl))))

(defun game-read ()
   (let ((cmd (read-from-string
      (concatenate 'string "(" (read-line) ")"))))
  (flet ((quote-it (x)
           (list 'quote x)))
    (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
(if (member (car sexp) *allowed-commands*)
         (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
    (when lst
    (let ((item (car lst)) (rest (cdr lst)))
    (cond ((eq item #\space) (cons item (tweak-text rest caps lit))) 
          ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) 
          ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
          ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit))) 
          (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)  t
                                      nil)
                  'string))
    (fresh-line))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))


(defparameter *max-label-length* 30)

(defun dot-label (exp)
          (if exp
  (let ((s (write-to-string exp :pretty nil)))
  (if (> (length s) *max-label-length*)
    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...") s))
""))

(defun nodes->dot (nodes) 
  (mapc (lambda (node) 
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];")) 
        nodes))

(defun edges->dot (edges)
     (mapc (lambda (node)
             (mapc (lambda (edge)
                     (fresh-line)
                     (princ (dot-name (car node)))
                     (princ "->")
                     (princ (dot-name (car edge)))
                     (princ "[label=\"")
                     (princ (dot-label (cdr edge)))
                     (princ "\"];"))
                   (cdr node)))
           edges))

(defun graph->dot (nodes edges) 
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

; ext:shell would be clisp specific, which i do not run..
;#+sbcl
;(defun shell (x)
;    (run-program "/bin/sh" (list "-c" x) :output t))
;
;(defun dot->png (fname thunk)
;  (with-open-file (*standard-output*
;                      fname
;                      :direction :output
;                      :if-exists :supersede)
;  (funcall thunk))
;(shell (concatenate 'string "dot -Tpng -O " fname)))
;(ext:shell (concatenate 'string "dot -Tpng -O " fname)))


(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
           fname
           :direction :output
           :if-exists :supersede)
    (funcall thunk))
  (sb-ext:run-program "/usr/local/bin/dot" (list "-Tpng" "-O" fname)))

(with-open-file (my-stream "testfile.txt"
                    :direction :output
                    :if-exists :supersede)
      (princ "Hello File!" my-stream))

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(defun uedges->dot (edges) 
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                          (fresh-line)
                          (princ (dot-name (caar lst)))
                          (princ "--")
                          (princ (dot-name (car edge)))
                          (princ "[label=\"")
                          (princ (dot-label (cdr edge)))
                          (princ "\"];")))
                      (cdar lst)))
              edges))

(defun ugraph->dot (nodes edges) 
  (princ "graph{")
     (nodes->dot nodes)
     (uedges->dot edges)
     (princ "}"))

(defun ugraph->png (fname nodes edges) 
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))
