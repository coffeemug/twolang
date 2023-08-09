(defpackage :twolang/lex/std-lex
  (:use :cl :maxpc :maxpc.char :maxpc.digit :twolang/util/maxpc)
  (:import-from :alexandria :flatten)
  (:export
   #:=std-token))

(in-package :twolang/lex/std-lex)

;; lexer
(defun =std-token ()
  (%or
   ;; literals
   (=lex/int-literal)
   (=lex/string-literal)

   ;; brackets
   (=lex/lparen)
   (=lex/rparen)
   (=lex/lcurly)
   (=lex/rcurly)

   ;; operators
   (=lex/plus)
   (=lex/minus)
   (=lex/asterisk)
   (=lex/slash)

   ;; other symbols
   (=lex/backtick)
   (=lex/colon)

   ;; words
   (=keyword)
   (=lex/ident)

   ;; whitespace
   (?ws)))

;; keywords
(defun =keyword ()
  (%or
   (=lex/fn)))

(deftoken+ fn (?string "fn"))

;; literals
(deftoken int-literal
    (=natural-number))

(deftoken string-literal
    (=destructure (_ str _)
		  (=list (?char #\") (=subseq (%any (?not (?char #\")))) (?char #\"))
      str))

(deftoken ident
    (%and (?not (=keyword))
	  (=transform
	   (=list (=alpha) (%any (=alnum)))
	   (lambda (elements)
	     (apply 'concatenate
		    (cons 'string (flatten elements)))))))


;; brackets
(deftoken+ lparen (?char #\())
(deftoken+ rparen (?char #\)))
(deftoken+ lcurly (?char #\{))
(deftoken+ rcurly (?char #\}))

;; operators
(deftoken+ plus (?char #\+))
(deftoken+ minus (?char #\-))
(deftoken+ asterisk (?char #\*))
(deftoken+ slash (?char #\/))

;; other symbols
(deftoken+ backtick (?char #\`))
(deftoken+ colon (?char #\:))

;; alphabet
(defun =alpha ()
  (=subseq (?satisfies 'alpha-char-p)))

(defun =digit ()
  (=subseq (?digit)))

(defun =alnum ()
  (%or (=alpha) (=digit)))


;; whitespace
(defun ?ws () (%some (?whitespace)))

