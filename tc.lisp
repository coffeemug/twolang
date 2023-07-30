(defpackage :twolang/tc
  (:use :cl :twolang/util/ast)
  (:export
   #:tc!))

(in-package :twolang/tc)

(defun tc! (node)
  (case (node node)
    (:int-literal (setf (node-type node) :int))
    (:addop (tc-op! node))
    (:subop (tc-op! node))
    (:mulop (tc-op! node))
    (otherwise nil))
  node)

(defun tc-op! (node)
  (tc! (node-left node))
  (tc! (node-right node))
  (unless (equal (node-type (node-left node))
		 (node-type (node-right node)))
    (error "op nodes have different types"))
  (unless (equal (node-type (node-left node))
		 :int)
    (error "op nodes have to be ints"))
  (setf (node-type node) :int))
