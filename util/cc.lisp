(defpackage :twolang/util/cc
  (:use :cl)
  (:export
   #:with-cc #:*cc-active-package* #:intern/cc))

(in-package :twolang/util/cc)

(defmacro with-cc (module-name &body body)
  `(let ((*cc-active-package* (make-package! ,module-name)))
     (declare (special *cc-active-package*))
     (setf *package* *cc-active-package*)
     ,@body))

(defun intern/cc (name)
  (declare (special *cc-active-package*))
  (intern (string-upcase name) *cc-active-package*))

(defun make-package! (name)
  "Makes and returns a package named `name`, or returns it if it
already exists."
  (let* ((upcased (string-upcase name))
	 (pkg (find-package upcased)))
    (or pkg (make-package upcased :use `(:cl)))))
