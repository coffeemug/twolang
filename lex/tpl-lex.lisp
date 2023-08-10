(defpackage :twolang/lex/tpl-lex
  (:use :cl :maxpc :maxpc.char :maxpc.digit :twolang/util/maxpc)
  (:import-from :twolang/lex/std-lex :=char/backtick)
  (:export
   #:=tpl-token))

(in-package :twolang/lex/tpl-lex)

;; lexer
(defun =tpl-token ()
  (%or
   (=char/backtick)
   (=char/interpol-start)
   (=char/template-substring)))

(deftoken+ interpol-start (?string "${"))

(deftoken template-substring
    (=subseq (%any (%and (?not (=char/interpol-start))
			 (?not (=char/backtick))))))

