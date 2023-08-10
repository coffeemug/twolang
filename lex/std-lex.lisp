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
   (=char/int-literal)
   (=char/string-literal)

   ;; brackets
   (=char/lparen)
   (=char/rparen)
   (=char/lcurly)
   (=char/rcurly)

   ;; operators
   (=char/plus)
   (=char/minus)
   (=char/asterisk)
   (=char/slash)

   ;; other symbols
   (=char/backtick)
   (=char/colon)

   ;; words
   (=keyword)
   (=char/ident)

   ;; whitespace
   (?ws)))

;; keywords
(defun =keyword ()
  (%or
   (=char/fn)))

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

