
(defmacro with-collectors (collectors &body body)
  (let (;; Create a gensym to name a lexical binding
	;; for each collector
	(bindings 
	 (mapcar (lambda (collector)
		   (declare (ignore collector))
		   (gensym))
		 collectors)))
    `(let ,bindings 
      (flet
	  ;; Names of collectors used to name functions
	  ;; that push object onto collector when called as
	  ;; (NAME-OF-COLLECTOR OBJECT)
	  ,(mapcar (lambda (collector binding)
		     `(,collector (obj) (push obj ,binding)))
		   collectors
		   bindings)
	,@body
	;; Return all collectors
	(values ,@(mapcar (lambda (binding) `(nreverse ,binding)) bindings))))))


(defmacro with-collect (&body body)
  `(with-collectors (collect)
     ,@body))

(defvar c
  `(with-collectors (even odd)
     (dolist (x '(1 2 3 4 5 6 7 8 9 10))
       (if (evenp x)
	   (even x)
	   (odd x)))))

(defvar d
  `(with-collect
     (dolist (x '(1 2 3 4 5 6 7 8 9 10))
       (if (evenp x)
	   (collect x)))))
