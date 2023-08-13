(defpackage :twolang/ast/funcall
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface :twolang/lex/std-lex)
  (:export #:funcall #:=funcall #:make-funcall))

(in-package :twolang/ast/funcall)

(defclass funcall (node)
  ((value :initarg :value :accessor funcall-value)))

(defun make-funcall (value pos)
  (make-instance 'funcall
		 :value value
		 :pos pos))

(defmethod tc! ((node funcall))
  (setf (node-type node) :int)
  node)

(defmethod cc ((node funcall))
  (funcall-value node))

(defun =funcall ()
  (=transform (=lex/int-literal)
	      (lambda (x)
		(make-funcall (lex-value x) "TODO"))))

