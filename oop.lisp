(in-package #:com.helmutkian.utils.oop)

;;; ************************************************************
;;; DEFCLASS*
;;; ************************************************************

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
			      ,(if (eql (second type) 'boolean)
				   (alexandria:symbolicate slot-name '-p)
				   (alexandria:symbolicate *accessor-prefix*
							   slot-name))))))
	 (alloc (get-slot-opt :allocation slot-form))
	 (initform (or (get-slot-opt :initform slot-form)
		       (unless (slot-opt-p (second slot-form))
			 `(:initform ,(second slot-form)))))
	 (initarg (unless (or (find :initarg unbound) initform)
		    (or (get-slot-opt :initarg slot-form)
			`(:initarg ,(intern (string slot-name) "KEYWORD")))))
	 (doc (get-slot-opt :documentation slot-form)))
    (apply #'append
	   `(,slot-name)
	   (remove nil
		   `(,reader
		     ,writer
		     ,accessor
		     ,alloc
		     ,initarg
		     ,initform
		     ,type
		     ,doc)))))
	 

(defmacro defclass* (name supers slots &rest options)
  `(defclass ,name ,supers
     ,(mapcar #'parse-slot slots)
     ,@options))

;;; ************************************************************
;;; SINGLETON
;;; ************************************************************

(defclass* singleton (standard-cass)
  ((instance :initform nil)))

(defmethod make-instance ((class singleton) &key)
  (or (instance singleton)
      (setf (instance singleton) (call-next-method))))

(defmethod validate-superclass ((sub singleton) (super standard-class))
  "SINGLETON can inherit from STANDARD-CLASS"
  t)

(defmethod validate-superclass ((sub singleton) (super singleton))
  "SINGLETON can inherit from SINGLETON"
  t)

(defmethod validate-superclass ((sub standard-class) (super singleton))
  "STANDARD-CLASS cannot inherit from SINGLETON"
  nil)

(defmacro defsingleton (name supers slots &rest options)
  `(defclass ,name ,supers
     ,slots
     ,@options
     (:metaclass singleton)))

;;; ************************************************************
;;; MIXIN
;;; ************************************************************

(defclass mixin (standard-class) ())

(defmethod make-instance ((class mixin) &key)
  (error "Cannot instantiate ~A mixin" (class-name class)))

(defmethod validate-superclass ((sub mixin) (super standard-class))
  t)

(defmethod validate-superclass ((sub mixin) (super mixin))
  t)

(defmethod validate-superclass ((sub standard-class) (super mixin))
  t)

(defmacro defmixin (name supers slots &rest options)
  `(defclass ,name ,supers
     ,slots
     ,@options
     (:metaclass mixin)))


;;; ************************************************************
;;; FINAL
;;; ************************************************************

(defclass final (standard-class) ())

(defmethod validate-superclass ((class final) (super standard-class))
  t)

(defmethod validate-superclass ((class standard-class) (super final))
  (error "Cannot subclass FINAL CLASS ~A" (class-name super)))

(defmacro definal (name supers slots &rest options)
  `(defclass ,name ,supers
     ,slots
     ,@options
     (:metaclass final)))

;;; ************************************************************
;;; DEFMETHOD*
;;; ************************************************************

;;; Utils from PG's "On Lisp"

	      
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
	     (lambda (&rest args)
	       (apply #'rmapcar fn args))
	     args)))

(defun explode (sym)
  (map 'list
       (lambda (c)
	 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))


;;; Expanders

(defun expand-accessor (accessor)
  "Expands to the function application form used for
   accessors and readers."
  `(,accessor self))

(defun expand-slot (slot)
  "Expands to a call to SLOT-VALUE to reference specified
   slot."
  `(slot-value self ',slot))


(defun expand-method-body (body)
  "Expands symbols prefixed with @ or $ to accessor or slot form 
   respectively"
  (rmapcar (lambda (x)
	     (let ((sym (when (symbolp x) (explode x))))
	       (cond
		 ;; expand "accessor": @sym => (sym self)
		 ((and sym (eql (car sym) '@))
		  (expand-accessor (apply #'symbolicate (cdr sym))))
		 ;; expand "slot": $sym => (slot-value self 'sym)
		 ((and sym (eql (car sym) '$))
		  (expand-slot (apply #'symbolicate (cdr sym))))
		 ;; expand else: sym => sym
		 (t 
		  x))))
	       body))


;;; Macro

(defun parse-method-definition (method-def)
  "Parses the method definition into name, combination specifier,
   arg list, declarations, docstring, and body returned as VALUES
   in that order."
  (let* ((name (pop method-def))
	 (combo 
	  (when (symbolp (car method-def))
	    (pop method-def)))
	 (args (pop method-def)))
    (multiple-value-bind (body decls docstr) (parse-body method-def)
      (values name
	      combo
	      args
	      decls
	      docstr
	      body))))

(defmacro defmethod* (&body body)
  (multiple-value-bind  (name combo args decls docstr meth-body)
      (parse-method-definition body)
    (remove nil
	    `(defmethod ,name ,combo ,args
			,docstr
			,@decls
		        ,@(expand-method-body meth-body)))))


;;; ************************************************************
;;; DEFPROTOCOL
;;; ************************************************************

(defmacro defprotocol (common-args &rest gen-fns)
  `(progn
     ,(loop for gen-fn in gen-fns
	    for name = (first gen-fn)
	    for args = (second gen-fn)
	    for doc = (or (third gen-fn) "")
	    collect `(defgeneric ,name ,(append common-args args)
		       (:documentation ,doc)))))