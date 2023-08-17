(defpackage :twolang/util/types
  (:use :cl :twolang/util/maxpc)
  (:export
   #:type-hrepr #:upident-to-type))

(in-package :twolang/util/types)

(defun type-hrepr (type)
  (cond
    ((keywordp type) (type-basic-hrepr type))
    ((listp type) (type-compound-hrepr type))
    (t (error "hrepr: unknown type"))))

(defun type-basic-hrepr (type)
  (string-capitalize
   (string-downcase
    (symbol-name type))))

(defun type-compound-hrepr (type)
  (cond
    ((eq (car type) :fn) (type-fn-hrepr type))
    (t (error "hrepr: unknown compound type"))))

(defun type-fn-hrepr (type)
  (format nil "() -> ~a" (type-hrepr (caddr type))))

(defun upident-to-type (upident)
  (cond
    ((string= (lex-value upident) "Int") :int)
    ((string= (lex-value upident) "String") :string)
    (t (error "Uknown upident (upident-to-type)"))))
