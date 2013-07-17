
(defmacro recur (name args &body body)
  (let ((tag (gensym)))
    `(tagbody ,tag
	(macrolet ((,name () (go ,tag)))))))
#|

(recur foo ((xs '(1 2 3)))
  (if (null xs)
      #t
      (recur foo (cdr xs))))
|#
