
(defclass range ()
  ((start :reader range-start :initarg :start)
   (end :reader range-end :initarg :end)
   (step :reader range-step :initarg :step)))

(defun make-range (start &key end (step 1))
  (if (or (and (< step 1) (and end (< start end)))
	  (and end (>= start end)))
      (error "Invalid range.")
      (make-instance 'range
		     :start start
		     :end end
		     :step step)))

(defclass range-iterator (iterator-mixin) 
  ((current-value :accessor range-iterator-current-value
		  :initarg :current-value)))

(defmethod make-iterator ((the-range range))
  (make-instance 'range-iterator
		 :collection the-range
		 :current-value (range-start the-range)))

(defmethod iterator-get ((iter range-iterator))
  (range-iterator-current-value iter))

(defmethod iterator-next ((iter range-iterator))
  (incf (range-iterator-current-value iter)
	(range-step (iterator-collection iter))))

(defmethod iterator-has-next-p ((iter range-iterator))
  (let ((end (range-end (iterator-collection iter)))
	(step (range-step (iterator-collection iter)))
	(curr-val (range-iterator-current-value iter)))
    (or (null end)
	(not (= (+ curr-val step) end)))))

(defmethod iterator-empty-p ((iter range-iterator))
  (let ((end (range-end (iterator-collection iter)))
	(step (range-step (iterator-collection iter)))
	(curr-val (range-iterator-current-value iter)))
    (and end
	(= (+ curr-val step) end))))
