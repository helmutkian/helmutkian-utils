
(defclass vector-iterator (bidirectional-iterator-mixin)
  ((index :accessor iter-index :initform 0)))

(defmethod make-iterator ((collection vector))
  (make-instance 'vector-iterator))

(defmethod iterator-next ((iter vector-iterator))
  (incf (iter-index iter)))

(defmethod iterator-prev ((iter vector-iterator))
  (decf (iter-index)))

(defmethod iterator-get ((iter vector-iterator))
  (aref (collection iter) (iter-index iter)))

(defmethod (setf iterator-get) (value (iter vector-iterator))
  (setf (aref (collection iter) (iter-index iter))
	value))

(defmethod iterator-has-next-p ((iter vector-iterator))
  (< (iter-index iter) (1- (length (collection iter)))))

(defmethod iterator-has-prev-p ((iter vector-iterator))
  (> (iter-index iter) 0))

(defmethod iterator-empty-p ((iter vector-iterator))
  (>= (iter-index iter) (length (collection iter))))
