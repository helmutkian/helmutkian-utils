
(in-package :com.helmutkian.utils)

(defmacro -> (value &body body)
  `(progn
     ,(reduce (lambda (inner outer)
		(list* (car outer) inner (cdr outer)))
	      body
	      :initial-value value)))
  
(defmacro ->> (value &body body)
  `(progn
     ,(reduce (lambda (inner outer)
		(append outer (list inner)))
	      body
	      :initial-value value)))
