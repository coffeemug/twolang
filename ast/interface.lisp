(defpackage :twolang/ast/interface
  (:use :cl)
  (:export #:node #:node-type #:node-pos  #:tc! #:cc #:node-p))

(in-package :twolang/ast/interface)

(defclass node ()
  ((pos :initarg :pos :accessor node-pos)
   (type :accessor node-type)))

(defgeneric tc! (node))
(defgeneric cc (node))

(defgeneric node-p (node))

(defmethod node-p (node)
  nil)

(defmethod node-p ((node node))
  t)
