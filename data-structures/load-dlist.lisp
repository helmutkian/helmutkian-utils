(defun with-path (path &rest files)
  (mapcar (lambda (file) (concatenate 'string path file))
	  files))

(defvar *dependencies*
  (with-path "/home/hkian/lisp/utils/helmutkian-utils/"
    "setf-call/setf-call.lisp"
    "with-collectors/with-collectors.lisp"))

(defun load-dlist ()
  (mapcar #'load *dependencies*)
  (ql:quickload "lisp-unit")
  (use-package :lisp-unit))
