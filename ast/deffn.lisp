(defpackage :twolang/ast/deffn
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface :twolang/lex/std-lex
   :twolang/ast/block :twolang/util/cc :twolang/util/tc
   :twolang/util/env :twolang/ast/shared :twolang/util/types)
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
  (declare (special *tc-env*))
  (with-scope *tc-env*
    (loop for arg in (deffn-args node)
	  do (add-variable *tc-env* (lex-value (car arg)) (upident-to-type (cdr arg))))
    (let ((rettype (node-type (tc! (deffn-body node)))))
      (setf (node-type node) `(:fn () ,rettype)))
    node))

(defmethod cc ((node deffn))
  (declare (special *cc-env*))
  `(defun
       ,(intern/cc (lex-value (deffn-name node)))
       (,@(mapcar
	   (lambda (arg)
	     (intern/cc (lex-value (car arg))))
	   (deffn-args node)))
     ,(with-scope *cc-env*
	(loop for arg in (deffn-args node)
	      do (add-variable *tc-env* (car arg) (cdr arg)))
	(cc (deffn-body node)))))

(defun =deffn ()
  (=destructure (_ name args body)
		(=list (=lex/fn) (=varspec) (=arglist) (=block))
    (make-deffn name args body "TODO")))

(defun =arglist ()
  (=destructure (_ arglist _)
		(=list (=lex/lparen) (=list* (=arg) (=lex/comma)) (=lex/rparen))
    arglist))

(defun =arg ()
  (=destructure (arg _ type)
		(=list (=varspec) (=lex/colon) (=typespec))
    (cons arg type)))

