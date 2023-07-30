(defpackage :twolang/util/ast
  (:use :cl :twolang/util/dict)
  (:export
   #:node
   #:node-value
   #:node-left
   #:node-right
   #:node-type
   #:set-node-type))

(in-package :twolang/util/ast)

(defun node (node)
  (dict-get node :node))

(defun node-value (node)
  (dict-get node :value))

(defun node-left (node)
  (dict-get node :left))

(defun node-right (node)
  (dict-get node :right))

(defun node-type (node)
  (dict-get node :type))

(defun set-node-type (node type)
  (dict-set! node :type type))

(defsetf node-type set-node-type)

