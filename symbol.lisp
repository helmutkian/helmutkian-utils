
(in-package #:com.helmutkian.utils.symbol)

;;; ************************************************************
;;; EXPLODE
;;; ************************************************************

(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

;;; ************************************************************
;;; SEPARATE-SYMBOLS
;;; ************************************************************

(defun split-symbol (sym &key (delimiter '-))
  (mapcar #'symbolicate
	  (split-sequence (char (write-to-string delimiter) 0)
			  (write-to-string sym))))
	 
