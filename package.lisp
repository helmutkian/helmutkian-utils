
(defpackage #:com.helmutkian.utils.setf
  (:nicknames #:utils.setf)
  (:use #:common-lisp)
  (:export #:setf-call
	   #:setf-call*
	   #:setf-all))

(defpackage #:com.helmutkian.utils.thrush
  (:nicknames #:utils.thrush)
  (:use #:common-lisp)
  (:export #:->
	   #:->>))

(defpackage #:com.helmutkian.utils.functional
  (:nicknames #:utils.functional)
  (:use #:common-lisp)
  (:export ;; ******
           ;; Deprecated
           ;; *****
           #:curry
	   #:rcurry
	   #:compose
	   ;; *****
	   ;; *****
	   #:foldl
	   #:foldr
	   #:compose-call))

(defpackage #:com.helmutkian.utils.symbol
  (:nicknames #:utils.symbol)
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:symbolicate)
  (:export #:symb ; Deprecated
	   #:explode
	   #:separate-symbols))

(defpackage #:com.helmutkian.utils.lambda
  (:nicknames #:utils.lambda)
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:flatten
		#:symbolicate)
  (:import-from #:utils.symbol
		#:explode)
  (:import-from #:cl-accumulators
		#:with-accumulator)
  (:export #:fn
	   ;#:fn*
	   #:iglambda))
