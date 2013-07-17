
(in-package #:com.helmutkian.utils.symbol)

;;; ************************************************************
;;; SYMB
;;;
;;; Deprecated in favor of ALEXANDRIA:SYMBOLICATE 
;;; ************************************************************

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

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

(defun separate-symbols (syms &key (seperator '-))
  (apply #'symbolicate
	 (append (mappend (lambda (s) (list s seperator)) (butlast syms))
		 (last syms))))
