
(in-package #:com.helmutkian.utils.symbol)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))


(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

(defun separate-symbols (syms &key (seperator '-))
  (apply #'symb
	 (append (mappend (lambda (s) (list s seperator)) (butlast syms))
		 (last syms))))
