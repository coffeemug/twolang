(defpackage :twolang/util/dict
  (:use :cl)
  (:export
   #:dict #:dict-get #:dict-set! #:dict-del!))

(in-package :twolang/util/dict)

(defun dict (&rest items)
  "Create a new dictionary."
  items)

(defun dict-get (dict key)
  "Get the value for a key in the dictionary."
  (let ((index (position key dict :test 'equal)))
    (when index
      (nth (1+ index) dict))))

(defun dict-set! (dict key value)
  "Set the value for a key in the dictionary."
  (let ((index (position key dict :test 'equal)))
    (if index
        (setf (nth (1+ index) dict) value)
        (nconc dict (list key value))))
  dict)

(defun dict-del! (dict key)
  "Delete a key-value pair from a property list in place."
  (dict-set! dict key nil))

