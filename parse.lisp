(defpackage :twolang/parse
  (:use :cl :maxpc :twolang/lex)
  (:shadow #:parse)
  (:export
   #:parse))

(in-package :twolang/parse)

;; driver
(defun parse (tokens)
  (maxpc::parse tokens (=toplevel)))

(defun =toplevel ()
  (%or (=int-literal)))

