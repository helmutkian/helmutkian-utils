;(in-package :com.helmutkian.utils.bang)

(defmacro with-gensyms (names &rest body)
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))

(defmacro setf-call (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	       (,(car var) (funcall ,op ,access ,@args)))
       ,set)))

(defmacro setf-all (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
		       args)))))

(defmacro setf-not (&rest args)
  `(progn
     ,@(mapcar (lambda (arg) `(setf-call #'not ,arg))
	       args)))

#|
(defvar *destructive-lookup-table* (make-hash-table))

(defmacro defdestructive (fn nfn)
  `(setf (gethash #',fn *destructive-lookup-table*) #',nfn))

(defun destructive (fn)
  (gethash fn *destructive-lookup-table*))

(defun destructive-call (fn &rest args)
  (apply (destructive fn) args))


(defdestructive remove delete)
;(defdestructive car pop)
(defdestructive mapcar mapc)
(defdestructive reverse nreverse)

|#
