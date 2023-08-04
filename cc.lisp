(defpackage :twolang/cc
  (:use :cl :twolang/util/ast)
  (:export
   #:cc))

(in-package :twolang/cc)

(defun cc (node)
  (case (node node)
    (:int-literal (node-value node))
    (:string-literal (node-value node))
    (:tagged-template (cc-tagged-template node))
    (:template-literal (cc-template-literal node))
    (:template-substring (node-value node))
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

(defun cc-tagged-template (node)
  (let ((tag (node-value (node-tag node))))
    (cond
      ((string= tag "cl") (cc-cl-template (node-template node)))
      (t (error "unknown tag for template")))))

(defun cc-cl-template (node)
  `(read-from-string
    ,(cc-template-literal node)))

(defun cc-template-literal (node)
  `(concatenate 'string 
		,@(loop for x in (node-elems node)
			collect `(format nil "~a" ,(cc x)))))
