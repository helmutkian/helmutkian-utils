(in-package #:com.helmutkian.utils.predicate)

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

(defun space-char-p (c)
  "Tests to see if character is whitespace"
  (or (char= #\space c)
      (char= #\tab c)
      (char= #\newline c)
      (char= #\return c)))