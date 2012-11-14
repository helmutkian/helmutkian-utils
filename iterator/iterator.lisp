
(define-mixin-class iterator-mixin () 
  ((collection :initarg :collection :accessor iterator-collection)))

(defgeneric make-iterator (collection)
  (:documentation "Constructor of an external iterator on the given collection"))

(defgeneric iterator-next (iterator)
  (:documentation "Advances the given external iterator to the next element of the collection"))


(defgeneric iterator-get (iterator)
  (:documentation "Yields the element currently referred to by the external iterator"))

(defgeneric (setf iterator-get) (value iterator)
  (:documentation "Calls SETF on the current element in the collection"))

(defgeneric iterator-has-next-p (iterator)
  (:documentation "Determines whether there is another element in the collection after the current element returned by ITERATOR-GET."))

(defgeneric iterator-empty-p (iterator)
  (:documentation "Determines whether are are any elements in the iterators collection"))


