(defpackage :twolang/tc
  (:use :cl :twolang/util/ast)
  (:export
   #:tc!))

(in-package :twolang/tc)

(defun tc! (node)
  (case (node node)
    (:int-literal (setf (node-type node) :int))
    (otherwise (error "Unknown node type")))
  node)
