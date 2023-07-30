(defpackage :twolang/util/maxpc
  (:use :cl :maxpc :twolang/util/ast)
  (:import-from :alexandria :make-keyword)
  (:export
   #:=constant
   #:=satisfies
   #:=fold
   #:=foldr
   #:=list*
   #:deftoken
   #:deftoken+
   #:=operators
   #:=operators-r))

(in-package :twolang/util/maxpc)

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

(defmacro deftoken (name parser)
  "Defines a lexer to parse the token out of a character stream, and a
corresponding parser to parse the token out a token stream."
  (let* ((name-str (symbol-name name))
         (keyword-name (make-keyword name-str))
	 (lexer-name (intern (concatenate 'string "=" "LEX/" name-str)))
         (predicate-name (intern (concatenate 'string "=" name-str))))
    `(progn
       (defun ,lexer-name ()
         (=transform
          ,parser
          (lambda (res)
            (list :node ,keyword-name :value res))))
       (defun ,predicate-name ()
         (=satisfies (lambda (token)
                       (eq (node token) ,keyword-name))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',predicate-name)))))

(defmacro deftoken+ (name parser)
  "Same as `deftoken`, but simple tokens that can be expressed as a
symbol."
  (let* ((name-str (symbol-name name))
         (keyword-name (make-keyword name-str))
	 (lexer-name (intern (concatenate 'string "=" "LEX/" name-str)))
         (predicate-name (intern (concatenate 'string "=" name-str))))
    `(progn
       (defun ,lexer-name ()
         (=constant ,parser ,keyword-name))
       (defun ,predicate-name ()
         (=constant (?eq ,keyword-name) ,keyword-name))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',predicate-name)))))

(defun =operators (operand-parser &rest token-optype-pairs)
  (=fold operand-parser
	 (=destructure (optype rvalue)
		       (=list (apply '%or (loop for (token optype) on token-optype-pairs by 'cddr
						collect (=constant token optype)))
			      operand-parser)
	   (list :node :binop :op optype :right rvalue))
	 (lambda (lvalue partial-operator)
	   `(:left ,lvalue ,@partial-operator))))

(defun =operators-r (operand-parser &rest token-optype-pairs)
  (=foldr (=destructure (lvalue optype)
			(=list operand-parser
			       (apply '%or (loop for (token optype) on token-optype-pairs by 'cddr
						 collect (=constant token optype))))
	    (list :node :binop :op optype :left lvalue))
	  operand-parser
	  (lambda (partial-operator rvalue)
	    `(:right ,rvalue ,@partial-operator))))

