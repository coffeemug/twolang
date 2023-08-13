(defpackage :twolang/ast/tagged-template
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface
	:twolang/lex/std-lex :twolang/lex/tpl-lex :twolang/lex/lexed-input
	:twolang/ast/template-literal  :twolang/ast/shared)
  (:export #:tagged-template #:=tagged-template #:make-tagged-template))

(in-package :twolang/ast/tagged-template)

(defclass tagged-template (node)
  ((tag :initarg :tag :accessor tagged-template-tag)
   (template :initarg :template :accessor tagged-template-template)))

(defun make-tagged-template (tag template pos)
  (make-instance 'tagged-template
		 :tag tag
		 :template template
		 :pos pos))

(defmethod tc! ((node tagged-template))
  (setf (node-type node) :unknown)
  node)

(defmethod cc ((node tagged-template))
  (let ((tag (lex-value (tagged-template-tag node))))
    (cond
      ((string= tag "cl") (cc-cl-template (tagged-template-template node)))
      (t (error "unknown tag for template")))))

(defun cc-cl-template (node)
  (let (concatenated)
    (setf concatenated
	  (apply #'concatenate 'string 
		 (loop for x in (template-literal-elems node)
		       collect (format nil "~a" (cc x)))))
    (unless (string= concatenated "")
      (read-from-string concatenated))))

(defun =tagged-template ()
  (=destructure (ident tmpl)
		(=list (=lex/downident) (=template-literal))
    (make-tagged-template ident tmpl "TODO")))

