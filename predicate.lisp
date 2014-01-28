(in-package #:com.helmutkian.utils.predicate)

;;; ************************************************************
;;; ************************************************************

(defun or-fn (&rest preds)
  "Returns a closure that serves as a union of all the given predicates,
   i.e. combining them all with OR."
  (lambda (&rest args)
    (loop for pred in preds
	  for result = (apply pred args)
	  when result return result)))

(defun and-fn (&rest preds)
  "Returns a closure that serves as an interestion of all the given predicates,
   i.e. combining them all with AND"
  (lambda (&rest args)
    (loop for pred in preds
	  for result = (apply pred args)
	  when (not result) return nil
	  finally (return result))))

;;; ************************************************************
;;; ************************************************************

(defun space-char-p (c)
  "Tests to see if character is whitespace"
  (or (char= #\space c)
      (char= #\tab c)
      (char= #\newline c)
      (char= #\return c)))

;;; ************************************************************
;;; Length predicates to complement ALEXANDRIA:LENGTH=
;;; ************************************************************

(defun length> (x y)
  (let ((xlen (if (integerp x) x (length x)))
	(ylen (if (integerp y) y (length y))))
    (assert (and (>= xlen 0) (>= ylen 0)))
    (> xlen ylen)))

(defun length<(x y)
  (let ((xlen (if (integerp x) x (length x)))
	(ylen (if (integerp y) y (length y))))
    (assert (and (>= xlen 0) (>= ylen 0)))
    (< xlen ylen)))

 (defun length>= (x y)
   (or (length= x y)
       (length> x y)))

 (defun length<= (x y)
  (or (length= x y)
      (length< x y)))

;;; ************************************************************
;;; Range predicates for different data types
;;; ************************************************************

(defgeneric in-range-p (object start end &key include-start include-end))

(defmethod in-range-p ((x number) start end &key (include-start t) include-end)
  (and (funcall (if include-start #'>= #'>) x start)
       (funcall (if include-end #'<= #'<) x end)))

(defmethod in-range-p ((c character) start end &key (include-start t) include-end)
  (and (funcall (if include-start #'char>= #'char<) c start)
       (funcall (if include-end #'char<= #'<) c end)))

(defmethod in-range-p ((s string) start end &key (include-start t) include-end)
  (and (funcall (if include-start #'string>= #'string>) s start)
       (funcall (if include-start #'string<= #'string<) s end)))

;;; ************************************************************
;;; ************************************************************