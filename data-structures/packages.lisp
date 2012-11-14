
(defpackage #:com.helmutkian.data-structures.dlist
  (:use #:common-lisp
	#:com.helmutkian.iterator)
  (:import-from #:com.helmutkian.utils.setf-call
		#:setf-all)
  (:import-from #:com.helmutkian.utils.with-collectors
		#:with-collect)
  (:export #:dlist
	   #:make-dlist
	   #:dlist-front-elt
	   #:dlist-back-elt
	   #:dlist-elt
	   #:dlist-empty-p
	   #:dlist-length
	   #:dlist-insert-front
	   #:dlist-insert-back
	   #:dlist-insert-before
	   #:dlist-insert-after
	   #:dlist-remove-if
	   #:dlist-remove
	   #:dlist-remove-at))
