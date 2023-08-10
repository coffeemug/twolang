(defpackage :twolang/util/types
  (:use :cl)
  (:export
   #:type-hrepr))

(in-package :twolang/util/types)

(defun type-hrepr (type)
  (string-capitalize
   (string-downcase
    (symbol-name type))))
