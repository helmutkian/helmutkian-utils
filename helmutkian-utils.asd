
(asdf:defsystem #:helmutkian-utils
  :depends-on ((:system "alexandria")
	       (:system "cl-accumulators"))
  :components ((:file "packages"
		      :depends-on ("alexandria"))
	       (:file "setf-utils"
		      :depends-on ("package"))
	       (:file "thrush"
		      :depends-on ("package"))
	       (:file "functional"
		      :depends-on ("package"))
	       (:file "symbol"
		      :depends-on ("package"))
	       (:file "lambda"
		      :depends-on ("package"
				   "symbol"
				   "cl-accumulators"))))

