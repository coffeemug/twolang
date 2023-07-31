
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

(test basics
  (evals-to "1" 1 :int)
  (evals-to "2 + 3 * 4" 14 :int)
  (evals-to "\"hello\"" "hello" :string))

;; 5am test on C-c C-c config
(setf fiveam:*run-test-when-defined* t)
