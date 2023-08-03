(defpackage :twolang/lex/std-lex
  (:use :cl :maxpc :maxpc.char :maxpc.digit :twolang/util/maxpc)
  (:export
   #:=token))

(in-package :twolang/lex/std-lex)

;; lexer
(defun =token ()
  (%or
   ;; literals
   (=lex/int-literal)
   (=lex/string-literal)

   ;; brackets
   (=lex/lparen)
   (=lex/rparen)

   ;; operators
   (=lex/plus)
   (=lex/minus)
   (=lex/asterisk)
   (=lex/slash)

   ;; whitespace
   (?ws)))

;; literals
(deftoken int-literal
    (=natural-number))

(deftoken string-literal
    (=destructure (_ str _)
		  (=list (?char #\") (=subseq (%any (?not (?char #\")))) (?char #\"))
      str))

;; brackets
(deftoken+ lparen (?char #\())
(deftoken+ rparen (?char #\)))

;; operators
(deftoken+ plus (?char #\+))
(deftoken+ minus (?char #\-))
(deftoken+ asterisk (?char #\*))
(deftoken+ slash (?char #\/))

;; whitespace
(defun ?ws () (%some (?whitespace)))

