(defpackage :twolang/ast/deffn
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface :twolang/lex/std-lex
   :twolang/ast/block :twolang/util/cc)
  (:export #:deffn #:=deffn #:make-deffn))

(in-package :twolang/ast/deffn)

(defclass deffn (node)
  ((name :initarg :name :accessor deffn-name)
   (args :initarg :args :accessor deffn-args)
   (body :initarg :body :accessor deffn-body)))

(defun make-deffn (name args body pos)
  (make-instance 'deffn
		 :name name
		 :args args
		 :body body
		 :pos pos))

(defmethod tc! ((node deffn))
  (let ((rettype (node-type (tc! (deffn-body node)))))
    (setf (node-type node) `(:fn () ,rettype)))
  node)

(defmethod cc ((node deffn))
  `(defun ,(intern/cc (lex-value (deffn-name node))) (#|,@(node-args node)|#)
     ,(cc (deffn-body node))))

(defun =deffn ()
  (=destructure (_ name args body)
		(=list (=lex/fn) (=lex/ident) (=arglist) (=block))
    (make-deffn name args body "TODO")))

(defun =arglist ()
  (=destructure (_ arglist _)
		(=list (=lex/lparen) (%any (=arg)) (=lex/rparen))
    arglist))

(defun =arg ()
  (=destructure (arg _ type)
		(=list (=lex/ident) (=lex/colon) (=typespec))
    (cons arg type)))

(defun =typespec ()
  (=lex/ident))

