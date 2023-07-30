(defpackage :twolang/parse
  (:use :cl :maxpc :twolang/lex :twolang/util/maxpc)
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
  (=operators (=int-literal)
	      (=asterisk) :mulop))
