(defpackage :twolang/cc
  (:use :cl :twolang/util/ast)
  (:export
   #:cc))

(in-package :twolang/cc)

(defun cc (node)
  (case (node node)
    (:int-literal (node-value node))
    (:addop (cc-op node))
    (:subop (cc-op node))
    (:mulop (cc-op node))
    (otherwise (error "cc unknown node type"))))

(defun cc-op (node)
  (let ((left/cc (cc (node-left node)))
	(right/cc (cc (node-right node))))
    (case (node node)
      (:addop `(+ ,left/cc ,right/cc))
      (:subop `(- ,left/cc ,right/cc))
      (:mulop `(* ,left/cc ,right/cc))
      (otherwise (error "cc-op unknown node type")))))
