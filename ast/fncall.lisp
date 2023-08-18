(defpackage :twolang/ast/fncall
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface
	:twolang/ast/var :twolang/lex/std-lex :twolang/ast/shared
	:twolang/util/types :twolang/util/tc :twolang/util/env)
  (:export #:fncall #:=fncall #:make-fncall))

(in-package :twolang/ast/fncall)

(defclass fncall (node)
  ((fn :initarg :fn :accessor fncall-fn)
   (args :initarg :args :accessor fncall-args)))

(defun make-fncall (fn args pos)
  (make-instance 'fncall
		 :fn fn
		 :args args
		 :pos pos))

(defmethod tc! ((node fncall))
  (declare (special *tc-env*))
  (with-scope *tc-env*
    (setf (node-type node) :int)
    node))

(defmethod cc ((node fncall))
  `(,(cc (fncall-fn node))
    ,@(loop for x in (fncall-args node)
	    collect (cc x))))

(defun =fncall ()
  (=destructure (name args)
		(=list (=var) (=arglist))
    (make-fncall name args "TODO")))

(defun =arglist ()
  (=destructure (_ arglist _)
		(=list (=lex/lparen) (=list* (=arg) (=lex/comma)) (=lex/rparen))
    arglist))

(defun =arg ()
  (=term/parser))

(defun =term/parser ()
  (find-symbol "=TERM/PARSER" (find-package :twolang/parse)))
