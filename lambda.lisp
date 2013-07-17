
(in-package #:com.helmutkian.utils.lambda)

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
    
