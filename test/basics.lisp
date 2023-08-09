
(defpackage :twolang/test/basics
  (:use :cl :fiveam :twolang/eval))

(in-package :twolang/test/basics)

(def-suite* basic-test-suite)

(defun evals-to (source &optional expected-result expected-type)
  (multiple-value-bind (res type)
      (eval-source source)
    (when expected-result
      (is (equal res expected-result)))
    (when expected-type
      (is (equal type expected-type)))))

(test primitives
  (evals-to "1" 1 :int)
  (evals-to "\"hello\"" "hello" :string)
  (evals-to "`${1} ${2}`" "1 2" :string))

(test arithmetic
  (evals-to "2 + 3 * 4" 14 :int))

(test cl-literal
  (evals-to "cl`(+ 1 2)`" 3 :unknown)
  (evals-to "cl`(+ 1 ${cl`2`})`" 3 :unknown))

(test block
  (evals-to "{ 3 4 }" 4 :int))

(test deffn
  (evals-to "fn constant(a: Int) { 1 }" :fn '(:fn :args (:int) :ret :int)))

;; 5am test on C-c C-c config
(setf fiveam:*run-test-when-defined* t)
