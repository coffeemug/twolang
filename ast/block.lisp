(defpackage :twolang/ast/block
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface :twolang/lex/std-lex)
  (:export #:block* #:=block #:make-block))

(in-package :twolang/ast/block)

(defclass block* (node)
  ((stmt-exprs :initarg :stmt-exprs :accessor block-stmt-exprs)))

(defun make-block (stmt-exprs pos)
  (make-instance 'block*
		 :stmt-exprs stmt-exprs
		 :pos pos))

(defmethod tc! ((node block*))
  (setf (node-type node)
	(or (loop for x in (block-stmt-exprs node)
		  for y = (tc! x)
		  finally (return
			    (when y
			      (node-type y))))
	    :!))
  node)

(defmethod cc ((node block*))
  (let ((stmt-exprs))
    (setf stmt-exprs
	  (loop for x in (block-stmt-exprs node)
		collect (cc x)))
    `(progn ,@stmt-exprs)))

(defun =block ()
  (=destructure (_ stmt-exprs _)
		(=list (=lex/lcurly) (%any (=term/parser)) (=lex/rcurly))
    (make-block stmt-exprs "TODO")))

(defun =term/parser ()
  (find-symbol "=TERM/PARSER" (find-package :twolang/parse)))
