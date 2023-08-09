(defpackage :twolang/tc
  (:use :cl :twolang/util/ast)
  (:export
   #:tc!
   #:type-hrepr))

(in-package :twolang/tc)

(defun tc! (node)
  (case (node node)
    (:int-literal (setf (node-type node) :int))
    (:string-literal (setf (node-type node) :string))
    (:deffn (tc-deffn! node))
    (:block (tc-block! node))
    (:tagged-template (setf (node-type node) :unknown))
    (:template-literal (setf (node-type node) :string))
    (:template-substring (setf (node-type node) :string))
    (:addop (tc-op! node))
    (:subop (tc-op! node))
    (:mulop (tc-op! node))
    (otherwise (error (format nil "tc! unknown node type ~a" (node node)))))
  node)

(defun tc-deffn! (node)
  (setf (node-type node)
	(tc-block! (node-body node))))

(defun tc-block! (node)
  (setf (node-type node)
	(or (loop for x in (node-elems node)
		  for y = (tc! x)
		  finally (return (node-type y)))
	    :!)))

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

(defun type-hrepr (type)
  (string-capitalize
   (string-downcase
    (symbol-name type))))
