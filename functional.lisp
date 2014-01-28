
(in-package #:com.helmutkian.utils.functional)


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

(defun flip (f)
  (lambda (&rest args)
    (apply f (nreverse args))))

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

;;; ************************************************************
;;; ************************************************************

(defparameter *ignore-arg-sym* '_)

(defun collect-ignore-args (args)
  "Helper function for IGLAMBDA that collects all the to-be-ignored
   arguments and returns two values: the lambda-list and the 
   generated symbols to-be-ignored"
  (loop for arg in args
        for arg* = (if (eql arg *ignore-arg-sym*) arg)
        collect arg* into lambda-list
        when (eql arg *ignore-arg-sym*) 
          collect arg* into ignore-args
        finally (return (values lambda-list ignore-args))))

(defmacro iglambda (args &body body)
  "TO-DO: Documentation"
  (multiple-value-bind (fn-body decls docstr)
      (tcr.parse-declarations-1.0::parse-body body)
    (multiple-value-bind (lambda-list ignore-args)
	(collect-ignore-args args)
      `(lambda ,lambda-list 
	 (declare (ignore ,@ignore-args))
	 ,@decls
	 ,@docstr
	 ,@fn-body))))
    
;;; ************************************************************
;;; ************************************************************

(defparameter *fn-implicit-arg-sym* '%)

(defmacro fn (&body body)
  `(lambda (,*fn-implicit-arg-sym*) ,@body))

(defmacro fn-call (fn-form arg)
  "Applies given FN form immediately on given ARG"
  `(funcall (fn ,fn-form) ,arg))