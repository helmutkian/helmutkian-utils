
(asdf:defsystem #:helmutkian-utils
  :depends-on ("alexandria" "split-sequence" "parse-declarations-1.0")
  :components ((:file "package")
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
				   "symbol"))
	       (:file "predicate"
		      :depends-on ("package"))))

