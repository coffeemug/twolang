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
   (=char/upident)
   (=char/downident)

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

(deftoken upident
    (%and (?not (=keyword))
	  (=transform
	   (=list (=upper) (%any (=alnum)))
	   (lambda (elements)
	     (apply 'concatenate
		    (cons 'string (flatten elements)))))))

(deftoken downident
    (%and (?not (=keyword))
	  (=transform
	   (=list (=lower) (%any (=alnum)))
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

(defun =upper ()
  (=subseq (?satisfies 'upper-case-p)))

(defun =lower ()
  (=subseq (?satisfies 'lower-case-p)))

(defun =digit ()
  (=subseq (?digit)))

(defun =alnum ()
  (%or (=alpha) (=digit)))


;; whitespace
(defun ?ws () (%some (?whitespace)))

