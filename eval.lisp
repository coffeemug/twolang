(defpackage :twolang/eval
  (:use :cl :twolang/util/ast :twolang/lex :twolang/lexed-input
	:twolang/parse :twolang/tc :twolang/cc :maxpc.input)
  (:export
   #:eval-source))

(in-package :twolang/eval)

(defun eval-source (source)
  (let* ((parsed (parse (make-lexed-input source (=token))))
	 (checked (tc! parsed))
	 (compiled (cc checked)))
    (values
     (funcall (compile nil `(lambda () ,compiled)))
     (node-type checked))))

