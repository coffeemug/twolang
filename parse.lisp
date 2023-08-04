(defpackage :twolang/parse
  (:use :cl :maxpc :twolang/lex/std-lex :twolang/lex/tpl-lex :twolang/lex/lexed-input :twolang/util/maxpc)
  (:shadow #:parse)
  (:export
   #:parse))

(in-package :twolang/parse)

;; driver
(defun parse (tokens)
  (maxpc::parse tokens (=toplevel)))

(defun =toplevel ()
  (%or (=term)))

(defun =term ()
  (=operators (=factor)
	      (=plus) :addop
	      (=minus) :subop))

(defun =factor ()
  (=operators (=primitive)
	      (=asterisk) :mulop))

(defun =primitive ()
  (%or
   (=int-literal)
   (=string-literal)
   (=tagged-template)
   (=template-literal)))

(defun =tagged-template ()
  (=destructure (ident tmpl)
		(=list (=ident) (=template-literal))
    (list :node :tagged-template :tag ident :template tmpl)))

(defun =template-literal ()
  (=destructure (_ elems _)
		(=list (%when (=backtick)
			      (lambda (input)
				(setf (active-lexer input)
				      (=tpl-token))))
		       (%any (%or (=interpol)
				  (=template-substring)))
		       (%when (=backtick)
			      (lambda (input)
				(setf (active-lexer input)
				      (=std-token)))))
    (list :node :template-literal :elems elems)))

(defun =interpol ()
  (=destructure (_ term _)
		(=list (%when (=interpol-start)
			      (lambda (input)
				(setf (active-lexer input)
				      (=std-token))))
		       '=term/parser
		       (%when (=rcurly)
			      (lambda (input)
				(setf (active-lexer input)
				      (=tpl-token)))))
    term))

(setf (fdefinition '=term/parser) (=term))
