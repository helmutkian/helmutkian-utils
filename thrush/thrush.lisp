
;(in-package :com.helmutkian.utils.thrush)


(defmacro -> (value &body body)
  `(progn
     ,(loop for form in (cons nil body)
	    for ->form = value
	      then (list* (first form) ->form (rest form))
	    finally (return ->form))))

(defmacro ->> (value &body body)
  `(progn
     ,(loop for form in (cons nil body)
	    for ->>form = value
	      then (append form (list ->>form))
	    finally (return ->>form))))
