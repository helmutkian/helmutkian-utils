
(defpackage #:com.helmutkian.utils.setf
  (:nicknames #:utils.setf)
  (:use #:common-lisp)
  (:export #:setf-call
	   #:setf-call*
	   #:setf-all))

(defpackage #:com.helmutkian.utils.thrush
  (:nicknames #:hk.utils.thrush)
  (:use #:common-lisp)
  (:export #:->
	   #:->>))

(defpackage #:com.helmutkian.utils.functional
  (:nicknames #:hk.utils.functional)
  (:use #:common-lisp)
  (:export #:foldl
	   #:foldr
	   #:filter
	   #:compose-call))

(defpackage #:com.helmutkian.utils.symbol
  (:nicknames #:hk.utils.symbol)
  (:use #:common-lisp #:split-sequence)
  (:import-from #:alexandria
		#:symbolicate)
  (:export #:explode
	   #:split-symbol))

(defpackage #:com.helmutkian.utils.lambda
  (:nicknames #:hk.utils.lambda)
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:flatten
		#:symbolicate)
  (:import-from #:hk.utils.symbol
		#:explode)
  (:import-from #:cl-accumulators
		#:with-accumulator)
  (:export #:fn
	   ;#:fn*
	   #:iglambda))
