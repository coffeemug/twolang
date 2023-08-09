(defpackage :twolang/util/ast
  (:use :cl :twolang/util/dict)
  (:export
   #:node
   #:node-value
   #:node-left
   #:node-right
   #:node-elems
   #:node-implicitp
   #:node-tag
   #:node-template
   #:node-name
   #:node-args
   #:node-body
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

(defun node-implicitp (node)
  (dict-get node :implicitp))

(defun node-tag (node)
  (dict-get node :tag))

(defun node-template (node)
  (dict-get node :template))

(defun node-name (node)
  (dict-get node :name))

(defun node-args (node)
  (dict-get node :args))

(defun node-body (node)
  (dict-get node :body))

(defun node-type (node)
  (dict-get node :type))

(defun set-node-type (node type)
  (dict-set! node :type type))

(defsetf node-type set-node-type)

