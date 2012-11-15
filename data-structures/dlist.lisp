

(defclass dcell ()
  ((prev :accessor dcell-prev :initarg :prev)
   (elt :accessor dcell-elt :initarg :elt)
   (next :accessor dcell-next :initarg :next)))

(defclass dlist ()
  ((front :accessor dlist-front-cell :initform nil)
   (back :accessor dlist-back-cell :initform nil)))

;;; **********************************************************************
;;; DCELL Protocol
;;; **********************************************************************

(defun make-dcell (elt &optional prev next)
  (make-instance 'dcell
		 :elt elt
		 :prev prev
		 :next next))

(defun dcell-insert-before (target-cell new-cell)
  "Inserts the new cell before the target-cell"
  (with-accessors ((target-prev dcell-prev)) target-cell
    (when target-prev
      (setf (dcell-next target-prev) new-cell))
    (setf (dcell-next new-cell) target-cell
	  (dcell-prev new-cell) target-prev
	  target-prev new-cell)))

(defun dcell-insert-after (target-cell new-cell)
  "Inserts the new cell after the target cell"
  (when (dcell-next target-cell)
    (setf (dcell-prev (dcell-next target-cell)) new-cell))
  (setf (dcell-next new-cell) (dcell-next target-cell)
	(dcell-next target-cell) new-cell
	(dcell-prev new-cell) target-cell))

(defun dcell-at-index (the-dlist index)
  "Retrieves the DCELL at the specified index within a DLIST"
  (do ((cell (dlist-front-cell the-dlist) (dcell-next cell))
       (i 0 (1+ i)))
      ((or (= index i) (null cell))
       (return-from dcell-at-index cell))))

(defun dcell-remove (target-cell)
  (with-accessors 
	((prev-cell dcell-prev) (next-cell dcell-next)) target-cell
    (when prev-cell
      (setf (dcell-next prev-cell) next-cell))
    (when next-cell
      (setf (dcell-prev next-cell) prev-cell))))

;;; **********************************************************************
;;; Non-Destructive DLIST Protocol
;;; **********************************************************************


(defun make-dlist ()
  "Constructs a new empty DLIST"
  (make-instance 'dlist))

(defun dlist-empty-p (the-dlist)
  "Determines whether a DLSIT is empty, ie contains no elements"
  (or (null (dlist-front-cell the-dlist))
      (null (dlist-back-cell the-dlist))))

(defun dlist-to-list (the-dlist)
  "Converts a DLIST to a LIST, preserving the front-to-back order of
the DLIST"
  (with-collect
   (do ((cell (dlist-front-cell the-dlist) (dcell-next cell)))
       ((null cell))
     (collect (dcell-elt cell)))))

(defun dlist-length (the-dlist)
  "Returns the length of the DLIST"
  (do ((cell (dlist-front-cell the-dlist) (dcell-next cell))
       (i 0 (1+ i)))
      ((null cell) i)))

(defun list-to-dlist (the-list)
  (let ((the-dlist (make-dlist)))
    (dolist (elm the-list)
      (dlist-insert-back elm the-dlist))
    the-dlist))

;;; **********************************************************************
;;; **********************************************************************

(defun dlist-front-elt (the-dlist)
  (dcell-elt (dlist-front-cell the-dlist)))

(defun dlist-back-elt (the-dlist)
  (dcell-elt (dlist-back-cell the-dlist)))

(defun dlist-elt (the-dlist index)
  (dcell-elt (dcell-at-index the-dlist index)))

(defun (setf dlist-front-elt) (new-elt the-dlist)
  (setf (dcell-elt (dlist-front-cell the-dlist)) new-elt))

(defun (setf dlist-back-elt) (new-elt the-dlist)
  (setf (dcell-elt (dlist-back-cell the-dlist)) new-elt))

(defun (setf dlist-elt) (new-elt the-dlist index)
  (setf (dcell-elt (dcell-at-index the-dlist index)) new-elt))


;;; **********************************************************************
;;; DLIST-INSERT internal generic function. See DLIST-INSERT generic
;;; function documentation for description.
;;;
;;; DLIST-INSERT-... is the external protocol
;;; **********************************************************************


(defgeneric dlist-insert (position object the-dlist &key index)
  (:documentation "
Encapsulates the insertion of new elements into a DLIST. This function is
destructive.

INPUT:
   position - A SYMBOL signifying the position for insertion. 
             Methods for FRONT, BACK, BEFORE, and AFTER are defined.
   object - The object to be inserted as a new element into the DLIST.
   the-dlist - The target DLIST to be inserted into
   index - for the BEFORE and AFTER specialized methods, indicates the index
           to insert before or after.
   
OUTPUT:
   Returns the modified DLIST"))

(defmethod dlist-insert :around (position object the-dlist &key index)
  "Traps calls to DLIST-INSERT assure proper insertion into an empty
DLIST and ensures the return value of the function as the modified
DLIST provided."
  (declare (ignore index))
  (if (dlist-empty-p the-dlist)
      (setf-all (make-dcell object)
	    (dlist-front-cell the-dlist)
	    (dlist-back-cell the-dlist))
      (call-next-method))
  the-dlist)

(defmethod dlist-insert ((position (eql 'front)) obj the-dlist &key index)
  "Inserts a new element at the front of a DLIST."
  (declare (ignore index))
  (let ((new-cell (make-dcell obj)))
    (dcell-insert-before (dlist-front-cell the-dlist)
			 new-cell)
    (setf (dlist-front-cell the-dlist) new-cell)))

(defmethod dlist-insert ((position (eql 'back)) obj the-dlist &key index)
  (declare (ignore index))
  "Inserts a new element at the back of a DLIST."
  (let ((new-cell (make-dcell obj))) 
    (dcell-insert-after (dlist-back-cell the-dlist)
			new-cell)
    (setf (dlist-back-cell the-dlist) new-cell)))


(defmethod dlist-insert ((position (eql 'before)) obj the-dlist &key index)
  "Inserts a new element at before the element at the given index."
  (if (zerop index)
      (dlist-insert 'front obj the-dlist)      
      (dcell-insert-before (dcell-at-index the-dlist index)
			   (make-dcell obj))))

(defmethod dlist-insert ((position (eql 'after)) obj the-dlist &key index)
  "Inserts a new element after the element at the given index"
  (let ((cell (dcell-at-index the-dlist index)))
    (if (or (null cell) (eql cell (dlist-back-cell the-dlist)))
	(dlist-insert 'back obj the-dlist)
	(dcell-insert-after cell (make-dcell obj)))))

;;; **********************************************************************
;;; DLIST-INSERT-... 
;;;
;;; DLIST-INSERT-... wraps calls to the generic function DLIST-INSERT
;;; appending the positions FRONT, BACK, BEFORE, and AFTER to the function
;;; name. This exposes a non-extensible external protocol for inserting 
;;; new elements into a DLIST
;;; **********************************************************************

(defun dlist-insert-front (obj the-dlist)
  "Inserts a new element at the front of a DLIST."
  (dlist-insert 'front obj the-dlist))

(defun dlist-insert-back (obj the-dlist)
  "Inserts a new element at the back of a DLIST."
  (dlist-insert 'back obj the-dlist))

(defun dlist-insert-before (obj index the-dlist)
  "Inserts a new element at before the element at the given index."
  (dlist-insert 'before obj the-dlist :index index))

(defun dlist-insert-after (obj index the-dlist)
  "Inserts a new element after the element at the given index"
  (dlist-insert 'after obj the-dlist :index index))


;;; **********************************************************************
;;; DLIST-REMOVE-... Protocol
;;; **********************************************************************

(defun dlist-remove-if (test the-dlist)
  (do ((cell (dlist-front-cell the-dlist) (dcell-next cell)))
      ((null cell) the-dlist)
    (when (funcall test (dcell-elt cell))
      (cond
	((eql cell (dlist-front-cell the-dlist))
	 (setf (dlist-front-cell the-dlist) 
	       (dcell-next cell)))
	((eql cell (dlist-back-cell the-dlist))
	 (setf (dlist-back-cell the-dlist)
	       (dcell-prev cell))))
      (dcell-remove cell))))

(defun dlist-remove (obj the-dlist &key (test #'eql))
  (dlist-remove-if (lambda (elt) (funcall test obj elt))
		   the-dlist))

(defun dlist-remove-at (index the-dlist)
  (let ((i -1))
    (dlist-remove-if (lambda (elt) 
		       (declare (ignore elt))
		       (= index (incf i)))
		     the-dlist)))



