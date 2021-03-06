
(defpackage #:com.helmutkian.utils.setf
  (:nicknames #:hk.utils.setf)
  (:use #:common-lisp)
  (:export #:setf-call
	   #:setf-call*
	   #:setf-all))

(defpackage #:com.helmutkian.utils.thrush
  (:nicknames #:hk.utils.thrush)
  (:use #:common-lisp)
  (:export #:->
	   #:->>))

(defpackage #:com.helmutkian.utils.symbol
  (:nicknames #:hk.utils.symbol)
  (:use #:common-lisp #:split-sequence)
  (:import-from #:alexandria
		#:symbolicate)
  (:export #:explode
	   #:split-symbol))

(defpackage #:com.helmutkian.utils.functional
  (:nicknames #:hk.utils.functional
	      #:hk.utils.fn)
  (:use #:common-lisp
	#:alexandria
	#:hk.utils.symbol)
  (:export #:rmapcar
	   #:rmapc
	   #:foldl
	   #:foldr
	   #:filter
	   #:compose-call
	   #:fn
	   #:%
	   #:fn-call
	   #:iglambda))

(defpackage #:com.helmutkian.utils.predicate
  (:nicknames #:hk.utils.predicate
	      #:hk.utils.pred
	      #:hk.utils.?)
  (:use #:common-lisp)
  (:export #:or-fn
	   #:and-fn
	   #:space-char-p
	   #:in-range-p))

(defpackage #:com.helmutkian.utils.oop
  (:nicknames #:hk.utils.oop)
  (:use #:common-lisp #:alexandria)
  (:import-from #:tcr.parse-declarations-1.0
		#:parse-body)
  (:export #:deflcass*
	   #:*accessor-prefix*
	   #:singleton
	   #:defsingleton
	   #:mixin
	   #:defmixin
	   #:final
	   #:definal))