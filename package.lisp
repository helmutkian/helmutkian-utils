
(defpackage #:com.helmutkian.utils
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:with-gensyms)
  (:export #:->
	   #:->>
	   #:_f
	   #:setf-all))
