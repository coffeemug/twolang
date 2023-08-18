(defpackage :twolang/ast/var
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface
	:twolang/lex/std-lex :twolang/util/tc :twolang/util/env
	:twolang/util/cc)
  (:export #:var #:=var #:make-var))

(in-package :twolang/ast/var)

(defclass var (node)
  ((name :initarg :name :accessor var-name)))

(defun make-var (name pos)
  (make-instance 'var
		 :name name
		 :pos pos))

(defmethod tc! ((node var))
  (declare (special *tc-env*))
  (let ((var (find-variable *tc-env* (var-name node))))
    (unless var
      (error (format nil "Can't find variable ~a" (var-name node))))
    (setf (node-type node) var)
    node))

(defmethod cc ((node var))
  (intern/cc (var-name node)))

(defun =var ()
  (=transform (=lex/downident)
	      (lambda (x)
		(make-var (lex-value x) "TODO"))))

