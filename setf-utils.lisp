
(in-package #:com.helmutkian.utils.setf)

(defmacro setf-call (op place &rest args)
  "(setf-call #'union foo '(a b c))
   => (setf foo (union foo '(a b c)))"
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	       (,(car var) (funcall ,op ,access ,@args)))
       ,set)))

(defmacro setf-all (val &rest args)
  (let ((gval (gensym)))
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
		       args)))))

(defmacro aif (test then else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

(defparameter *setf-place-symbol* '_)

(defun transform-funcall (the-funcall)
  (aif (member *setf-place-symbol* the-funcall)
    (let ((arg (gensym)))
      (assert (= (count *setf-place-symbol* it) 1)
	      (the-funcall)
	      "~S contains more than one instance of ~S"
	      the-funcall *setf-place-symbol*)
      (setf (car it) arg)
      `(lambda (,arg) ,the-funcall))
    the-funcall))
	 
      `
(defmacro setf-call* (the-funcall place)
  "(setf-call* (append '(1 2) _ '(3 4)) foo)
   => (setf-call (lambda (x) (append '(1 2) x '(3 4)))
                 foo)"
  `(setf-call ,(transform-funcall the-funcall) ,place))
