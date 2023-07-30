(defpackage :twolang/lex
  (:use :cl :maxpc :maxpc.char :maxpc.digit :access :twolang/util/maxpc)
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
  (%or (=lex/int-literal)
       (=bracket)
       (=operator)
       (?ws)))

;; literals
(deftoken int-literal
    (=natural-number))

;; brackets
(defun =bracket ()
  (%or (=lex/lparen)
       (=lex/rparen)))

(deftoken+ lparen (?char #\())
(deftoken+ rparen (?char #\)))

;; operators
(defun =operator ()
  (%or (=lex/plus)
       (=lex/minus)
       (=lex/asterisk)
       (=lex/slash)))

(deftoken+ plus (?char #\+))
(deftoken+ minus (?char #\-))
(deftoken+ asterisk (?char #\*))
(deftoken+ slash (?char #\/))

(defun ?ws () (%some (?whitespace)))

