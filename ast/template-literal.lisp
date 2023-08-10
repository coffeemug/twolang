(defpackage :twolang/ast/template-literal
  (:use :cl :maxpc :twolang/util/maxpc :twolang/ast/interface
	:twolang/lex/std-lex :twolang/lex/tpl-lex :twolang/lex/lexed-input
	:twolang/ast/string-literal)
  (:export #:template-literal #:=template-literal #:make-template-literal
	   #:template-literal-elems))

(in-package :twolang/ast/template-literal)

(defclass template-literal (node)
  ((value :initarg :elems :accessor template-literal-elems)))

(defun make-template-literal (elems pos)
  (make-instance 'template-literal
		 :elems elems
		 :pos pos))

(defmethod tc! ((node template-literal))
  (setf (node-type node) :string)
  node)

(defmethod cc ((node template-literal))
  `(concatenate 'string 
		,@(loop for x in (template-literal-elems node)
			collect `(format nil "~a" ,(cc x)))))

(defun =template-literal ()
  (=destructure (_ elems _)
		(=list (%when (=lex/backtick)
			      (lambda (input)
				(setf (active-lexer input)
				      (=tpl-token))))
		       (%any (%or (=interpol)
				  (=template-substring)))
		       (%when (=lex/backtick)
			      (lambda (input)
				(setf (active-lexer input)
				      (=std-token)))))
    (make-template-literal
     elems
     "TODO")))

(defun =interpol ()
  (=destructure (_ term _)
		(=list (%when (=lex/interpol-start)
			      (lambda (input)
				(setf (active-lexer input)
				      (=std-token))))
		       (=term/parser)
		       (%when (=lex/rcurly)
			      (lambda (input)
				(setf (active-lexer input)
				      (=tpl-token)))))
    term))

(defun =template-substring ()
  (=transform
   (=lex/template-substring)
   (lambda (ss)
     (make-string-literal (lex-value ss) "TODO"))))

(defun =term/parser ()
  (find-symbol "=TERM/PARSER" (find-package :twolang/parse)))

