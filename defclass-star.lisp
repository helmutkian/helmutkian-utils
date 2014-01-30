(in-package #:com.helmutkian.utils.defclass-star)

(defparameter *accessor-prefix* 'get-)

(defparameter *slot-options*
  '(:reader 
    :writer 
    :accessor 
    :allocation 
    :initarg 
    :initform 
    :type 
    :documentation
    :unbound))

(defun slot-opt-p (thing)
  (and (keywordp thing) (find thing *slot-options*)))

(defun get-slot-opt (opt slot-form)
  "If SLOT-FORM has slot option OPT, then return OPT and value in a LIST,
   otherwise return NIL"
  (let ((result (member opt slot-form)))
    (when result (subseq result 0 2))))


(defun parse-slot (slot)
  (let* ((slot-form (alexandria:ensure-list slot))
	 (slot-name (first slot-form))
	 (unbound (alexandria:ensure-list 
		   (second (get-slot-opt :unbound slot-form))))
	 (type (get-slot-opt :type slot-form))
	 (writer (get-slot-opt :writer slot-form))
	 (reader (get-slot-opt :reader slot-form))
	 (accessor (and (not (find :accessor unbound))
		        (not reader) 
			(not writer) 
			(or (get-slot-opt :accessor slot-form)
			    `(:accessor
			      ,(if (eql type 'boolean)
				   (alexandria:symbolicate slot-name '-p)
				   (alexandria:symbolicate *accessor-prefix*
							   slot-name))))))
	 (alloc (get-slot-opt :allocation slot-form))
	 (initform (or (get-slot-opt :initform slot-form)
		       (unless (slot-opt-p (second slot-form))
			 `(:initform ,(second slot-form)))))
	 (initarg (unless (find :initarg unbound)
		    (or (get-slot-opt :initarg slot-form)
			`(:initarg ,(intern (string slot-name) "KEYWORD")))))
	 (doc (get-slot-opt :documentation slot-form)))
    (remove nil
	    `(,slot-name
	      ,@reader
	      ,@writer
	      ,@accessor
	      ,@alloc
	      ,@initarg
	      ,@initform
	      ,@type
	      ,@doc))))
	 

(defmacro defclass* (name supers slots &rest options)
  `(defclass ,name ,supers
     ,(mapcar #'parse-slot slots)
     ,@options))

