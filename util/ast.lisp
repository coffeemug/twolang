(defpackage :twolang/util/ast
  (:use :cl :twolang/util/dict)
  (:export
   #:node
   #:node-value
   #:node-left
   #:node-right
   #:node-elems
   #:node-tag
   #:node-template
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

(defun node-elems (node)
  (dict-get node :elems))

(defun node-tag (node)
  (dict-get node :tag))

(defun node-template (node)
  (dict-get node :template))

(defun node-type (node)
  (dict-get node :type))

(defun set-node-type (node type)
  (dict-set! node :type type))

(defsetf node-type set-node-type)

