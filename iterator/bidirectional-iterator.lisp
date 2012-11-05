
(defmixin bidirectional-mixin () ())

(defgeneric iterator-prev (iterator)
  (:documentation "Reverses the given external iterator to the previous element of the collection"))

(defgeneric iterator-has-prev-p (iterator)
  (:documentation "Determines whether the given external iterator has an element previous the current one."))
