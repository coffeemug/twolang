(defpackage :twolang/util/maxpc
  (:use :cl :maxpc :twolang/util/dict)
  (:import-from :alexandria :make-keyword)
  (:export
   #:lex-value
   #:%when
   #:=constant
   #:=satisfies
   #:=fold
   #:=foldr
   #:=list*
   #:with-pos%
   #:deftoken
   #:deftoken+
   #:=operators
   #:=operators-r))

(in-package :twolang/util/maxpc)

(defun lex-value (node)
  (dict-get node :value))

(defun %when (parser callback)
  (lambda (input)
    (multiple-value-bind (rest result resultp)
	(funcall parser input)
      (when rest
	(funcall callback rest))
      (values rest result resultp))))

(defun =constant (parser result)
  (=transform parser
	      (lambda (&rest _)
		(declare (ignore _))
		result)))

(defun =satisfies (predicate &optional (map #'identity))
  (=transform
   (=subseq (?satisfies predicate))
   (lambda (x) (funcall map (car x)))))

(defun =fold (initial-parser next-parser fn)
  (=destructure (first rest)
		(=list initial-parser
		       (%any next-parser))
    (reduce fn (cons first rest))))

(defun =foldr (repeat-parser last-parser fn)
  (=destructure (repeat last)
		(=list (%any repeat-parser)
		       last-parser)
    (reduce fn `(,@repeat ,last) :from-end t)))

(defun =list* (item-parser delimeter-parser)
  "Delimited list parser"
  (%maybe
   (=destructure (head tail)
		 (=list item-parser
			(%any (=destructure (_ item)
					    (=list delimeter-parser item-parser)
					    item)))
     (cons head tail))))

(defmacro with-pos% (posvar parser)
  (let ((input-var (gensym)))
    `(lambda (,input-var)
       (let ((,posvar (maxpc.input:input-position ,input-var)))
	 (funcall
	  ,parser
	  ,input-var)))))

(with-pos% pos
  (=constant 1))

(defmacro deftoken (name parser)
  "Defines a lexer to parse the token out of a character stream, and a
corresponding parser to parse the token out a token stream."
  (let* ((name-str (symbol-name name))
         (keyword-name (make-keyword name-str))
	 (lexer-name (intern (concatenate 'string "=" "CHAR/" name-str)))
         (predicate-name (intern (concatenate 'string "=" "LEX/" name-str))))
    `(progn
       (defun ,lexer-name ()
	 (with-pos% pos
	   (=transform
	    ,parser
	    (lambda (res)
	      (list :node ,keyword-name
		    :value res
		    :pos pos)))))
       (defun ,predicate-name ()
         (=satisfies (lambda (token)
                       (eq (dict-get token :node) ,keyword-name))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',predicate-name)))))

(defmacro deftoken+ (name parser)
  "Same as `deftoken`, but simple tokens that can be expressed as a
symbol."
  `(deftoken ,name ,parser))
