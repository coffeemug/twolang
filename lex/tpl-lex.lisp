(defpackage :twolang/lex/tpl-lex
  (:use :cl :maxpc :maxpc.char :maxpc.digit :twolang/util/maxpc)
  (:import-from :twolang/lex/std-lex :=lex/backtick)
  (:export
   #:=tpl-token))

(in-package :twolang/lex/tpl-lex)

;; lexer
(defun =tpl-token ()
  (%or
   (=lex/backtick)
   (=lex/interpol-start)
   (=lex/template-substring)))

(deftoken+ interpol-start (?string "${"))

(deftoken template-substring
    (=subseq (%any (%and (?not (=lex/interpol-start))
			 (?not (=lex/backtick))))))

