(defpackage :twolang/ast/binop
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface :twolang/lex/std-lex)
  (:export #:binop #:=binop #:make-binop #:=operators #:=operators-r))

(in-package :twolang/ast/binop)

(defclass binop (node)
  ((op :initarg :op :accessor binop-op)
   (left :initarg :left :accessor binop-left)
   (right :initarg :right :accessor binop-right)))

(defun make-binop (op left right pos)
  (make-instance 'binop
		 :op op
		 :left left
		 :right right
		 :pos pos))

(defmethod tc! ((node binop))
  (tc! (binop-left node))
  (tc! (binop-right node))
  (unless (equal (node-type (binop-left node))
		 (node-type (binop-right node)))
    (error "op nodes have different types"))
  (unless (equal (node-type (binop-left node))
		 :int)
    (error "op nodes have to be ints"))
  (setf (node-type node) :int)
  node)

(defmethod cc ((node binop))
  (let ((left/cc (cc (binop-left node)))
	(right/cc (cc (binop-right node))))
    (case (binop-op node)
      (:add `(+ ,left/cc ,right/cc))
      (:sub `(- ,left/cc ,right/cc))
      (:mul `(* ,left/cc ,right/cc))
      (otherwise (error "cc-op unknown node type")))))

(defun =operators (operand-parser &rest token-optype-pairs)
  (=fold operand-parser
	 (with-pos% pos
	   (=destructure (optype rvalue)
			 (=list (apply '%or (loop for (token optype) on token-optype-pairs by 'cddr
						  collect (=constant token optype)))
				operand-parser)
	     (make-binop optype nil rvalue pos)))
	 (lambda (lvalue partial-operator)
	   (setf (binop-left partial-operator) lvalue)
	   partial-operator)))

(defun =operators-r (operand-parser &rest token-optype-pairs)
  (=foldr (with-pos% pos
	    (=destructure (lvalue optype)
			  (=list operand-parser
				 (apply '%or (loop for (token optype) on token-optype-pairs by 'cddr
						   collect (=constant token optype))))
	      (make-binop optype lvalue nil pos)))
	  operand-parser
	  (lambda (partial-operator rvalue)
	    (setf (binop-right partial-operator) rvalue)
	    partial-operator)))

