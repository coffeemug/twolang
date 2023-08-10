(defpackage :twolang/ast/int-literal
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface :twolang/lex/std-lex)
  (:export #:int-literal #:=int-literal #:make-int-literal))

(in-package :twolang/ast/int-literal)

(defclass int-literal (node)
  ((value :initarg :value :accessor int-literal-value)))

(defun make-int-literal (value pos)
  (make-instance 'int-literal
		 :value value
		 :pos pos))

(defmethod tc! ((node int-literal))
  (setf (node-type node) :int)
  node)

(defmethod cc ((node int-literal))
  (int-literal-value node))

(defun =int-literal ()
  (=transform (=lex/int-literal)
	      (lambda (x)
		(make-int-literal (lex-value x) "TODO"))))

