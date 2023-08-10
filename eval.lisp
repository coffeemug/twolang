(defpackage :twolang/eval
  (:use :cl :twolang/lex/std-lex :twolang/lex/lexed-input
	:twolang/parse :twolang/ast/interface :maxpc.input)
  (:export
   #:eval-source))

(in-package :twolang/eval)

(defun eval-source (source)
  (let* ((parsed (parse (make-lexed-input source (=std-token))))
	 (checked (tc! parsed))
	 (compiled (cc checked)))
    (values
     (funcall (compile nil `(lambda () ,compiled)))
     (node-type checked))))

