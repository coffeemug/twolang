(defpackage :twolang/ast/string-literal
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface :twolang/lex/std-lex)
  (:export #:string-literal #:=string-literal #:make-string-literal))

(in-package :twolang/ast/string-literal)

(defclass string-literal (node)
  ((value :initarg :value :accessor string-literal-value)))

(defun make-string-literal (value pos)
  (make-instance 'string-literal
		 :value value
		 :pos pos))

(defmethod tc! ((node string-literal))
  (setf (node-type node) :string)
  node)

(defmethod cc ((node string-literal))
  (string-literal-value node))

(defun =string-literal ()
  (=transform (=lex/string-literal)
	      (lambda (x)
		(make-string-literal (lex-value x) "TODO"))))

