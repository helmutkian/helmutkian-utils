
(defclass list-iterator (iterator-mixin) ())

(defmethod make-iterator (collection)
  (make-instance 'list-iterator
		 :collection collection))

(defmethod iterator-get ((iter list-iterator))
  (car (collection iter)))

(defmethod iterator-next ((iter list-iterator))
  (setf-call #'cdr (collection iter)))

(defmethod (setf iterater-get) (value (iter list-iterator))
  (setf (car (collection iterator)) value))

(defmethod iterator-has-next-p ((iter list-iterator))
  (not (null (cdr (collection iter)))))

(defmethod iterator-empty-p ((iter list-iterator))
  (null (collection iter)))
