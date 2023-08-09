(defpackage :twolang/parse
  (:use :cl :maxpc :maxpc.char :twolang/lex/std-lex :twolang/lex/tpl-lex :twolang/lex/lexed-input :twolang/util/maxpc)
  (:shadow #:parse)
  (:export
   #:parse))

(in-package :twolang/parse)

;; driver
(defun parse (tokens)
  (maxpc::parse tokens (=toplevel)))

(defun =toplevel ()
  (%or (=deffn)
       (=term)))

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
   (=template-literal)
   (=block)))

;; function definition
(defun =deffn ()
  (=destructure (_ name args body)
		(=list (=fn) (=ident) (=arglist) (=block))
    (list :node :deffn :name name :args args :body body)))

(defun =arglist ()
  (=destructure (_ arglist _)
		(=list (=lparen) (%any (=arg)) (=rparen))
    arglist))

(defun =arg ()
  (=destructure (arg _ type)
		(=list (=ident) (=colon) (=typespec))
    (cons arg type)))

(defun =typespec ()
  (=ident))

(defun =block (&key implicitp)
  (=destructure (_ stmt-exprs _)
		(=list (=lcurly) (%any '=term/parser) (=rcurly))
    (list :node :block :elems stmt-exprs :implicitp implicitp)))

;; template (literals and tagged)
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
