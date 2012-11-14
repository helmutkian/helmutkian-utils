
(defun collect-macro-forms (forms)
  (with-collectors (do-list-forms end-list-forms macrolet-forms)
    (dolist (form foms) 
      (let ((var)  (first form))
	    (coll (second form))
	    (sym (gensym)))
	(do-list-forms `(,sym (make-iterator ,coll)))
	(end-list-forms `(iterator-empty-p ,sym))
	(macrolet-forms `(,var () (iterator-get ,sym)))))))

(defmacro do-iterator (forms &body body)
  (multiple-value-bind (do-list-forms end-list-forms macrolet-forms)
      (collect-macro-forms forms)
    `(do ,do-list-forms
	 ((or ,@end-list-forms))
       (macrolet ,macrolet-forms
	,@body))))
