(defpackage :twolang/eval
  (:use :cl :twolang/util/ast :twolang/lex :twolang/parse :twolang/tc :twolang/cc)
  (:export
   #:eval-source))

(in-package :twolang/eval)

(defun eval-source (source)
  (let* ((lexed (lex source))
	 (parsed (parse lexed))
	 (checked (tc! parsed))
	 (compiled (cc checked)))
    (values
     (funcall (compile nil `(lambda () ,compiled)))
     (node-type checked))))

