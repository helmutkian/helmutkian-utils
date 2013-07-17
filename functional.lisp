
(in-package #:com.helmutkian.utils.functional)

;;; ************************************************************
;;; Deprecated for identitical functions in ALEXANDRIA package
;;; ************************************************************

(defun curry (fn &rest args)
  (lambda (&rest more-args)
    (apply fn (append args more-args))))

(defun rcurry (fn &rest args)
  (lambda (&rest more-args)
    (apply fn (append more-args args))))

(defun compose (&rest functions)
  (flet ((compose2 (f g)
	   (lambda (&rest args)
	     (funcall f (apply g args)))))
    (reduce #'compose2
	    functions
	    :from-end t)))

;;; ************************************************************
;;; ************************************************************

(defun foldl (function initval the-list)
  "Wrapper for (REDUCE <...> :INITIAL-VALUE <...> :FROM-END NIL) for
   the sake of a more familiar and terse syntax."
  (reduce function the-list :initial-value initval))

(defun foldr (function initval the-list)
  "Wrapper for (REDUCE <...> :INITIAL-VALUE <...> :FROM-END T) for
   the sake of a more familiar and terse syntax."
  (reduce function the-list :initial-value initval :from-end t))

(defun filter (predicate sequence &rest args)
  "Wrapper for (REMOVE-IF-NOT <...>) the sake of
   a more familiar and terse syntax"
  (apply #'remove-if-not predicate sequence args))

;;; ************************************************************
;;; ************************************************************

(defmacro compose-call (functions &rest args)
  "A macro which simplifies the calling of nested functions in terms of COMPOSE.
   (compose-call (#'f #'g #'h) x y z) 
   == (funcall (compose #'f #'g #'h) x y z)
   == (f (g (h x y z)))"
  (let ((first-call t))
   (foldr (lambda (f g)
	     `(funcall ,f
		       ,@(cond (first-call
				(setf first-call nil)
				`(,@g))
			       (t `(,g)))))
	  args 
	  functions)))
	   

#|

Function version of COMPOSE-CALL macro.

(defun compose-call (functions &rest args)
  (let ((first-arg t))
   (reduce (lambda (fn args)
	     (cond 
	       (first-arg (setf first-arg nil)
			  (apply fn args))
	       (t (funcall fn args))))
	  functions
	  :initial-value args
	  :from-end t)))
|#
