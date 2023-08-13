(defpackage :twolang/ast/shared
  (:use :cl :maxpc :twolang/util/maxpc :twolang/lex/std-lex)
  (:export #:=typespec #:=varspec))

(in-package :twolang/ast/shared)

(defun =typespec ()
  (=lex/upident))

(defun =varspec ()
  (=lex/downident))

