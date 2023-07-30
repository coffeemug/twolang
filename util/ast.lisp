(defpackage :twolang/util/ast
  (:use :cl :twolang/util/dict)
  (:export
   #:node
   #:node-value
   #:node-type
   #:set-node-type))

(in-package :twolang/util/ast)

(defun node (node)
  (dict-get node :node))

(defun node-value (node)
  (dict-get node :value))

(defun node-type (node)
  (dict-get node :type))

(defun set-node-type (node type)
  (dict-set! node :type type))

(defsetf node-type set-node-type)

