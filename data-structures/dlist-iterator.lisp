
(defclass dlist-iterator (bidirectional-iterator-mixin) ())

(defmethod make-iterator ((collection dlist))
  (make-instance 'dlist-iterator
		 :collection (dlist-front-cell dlist)))

(defmethod iterator-next ((iter dlist-iterator))
  (setf-call #'dcell-next (collection iter)))

(defmethod iterator-prev ((iter dlist-iterator))
  (setf-call #'dcell-prev (collection iter)))

(defmethod iterator-get ((iter dlist-iterator))
  (dcell-elt (collection iter)))

(defmethod (setf iterator-get) (value (iter dlist-iterator))
  (setf (dcell-elt (collection iter)) value))

(defmethod iterator-has-next-p ((iter dlist-iterator))
  (not (null (dcell-next (collection iter)))))

(defmethod iterator-has-prev-p ((iter dlist-iterator))
  (not (null (dcell-prev (collection iter)))))

(defmethod iterator-empty-p ((iter dlist-iterator))
  (null (collection iter)))
