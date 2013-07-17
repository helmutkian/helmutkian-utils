
(asdf:defsystem #:helmutkian-utils
  :components ((:file "packages")
	       (:file "setf-utils"
		      :depends-on ("package"))
	       (:file "thrush"
		      :depends-on ("package"))
	       (:file "functional"
		      :depends-on ("package"))
	       (:file "symbol"
		      :depends-on ("package"))
	       (:file "lambda"
		      :depends-on ("package"))))

