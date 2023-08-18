(defpackage :twolang/parse
  (:use :cl :maxpc :maxpc.char :twolang/lex/std-lex
	:twolang/lex/tpl-lex :twolang/lex/lexed-input :twolang/util/maxpc
   :twolang/ast/int-literal :twolang/ast/string-literal
   :twolang/ast/template-literal :twolang/ast/tagged-template
   :twolang/ast/block :twolang/ast/binop :twolang/ast/deffn
	:twolang/ast/var :twolang/ast/fncall)
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
	      (=lex/plus) :add
	      (=lex/minus) :sub))

(defun =factor ()
  (=operators (=primitive)
	      (=lex/asterisk) :mul))

(defun =primitive ()
  (%or
   (=int-literal)
   (=string-literal)
   (=tagged-template)
   (=template-literal)
   (=block)
   (=fncall)
   (=var)))

(setf (fdefinition '=term/parser) (=term))
