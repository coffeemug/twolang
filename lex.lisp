(defpackage :twolang/lex
  (:use :cl :maxpc :maxpc.char :maxpc.digit :twolang/util/maxpc)
  (:import-from :alexandria :flatten)
  (:export
   #:lex))

(in-package :twolang/lex)

;; lexer
(defun lex (str)
  (parse str (%tokens)))

(defun %tokens ()
  (%any (=token)))

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

