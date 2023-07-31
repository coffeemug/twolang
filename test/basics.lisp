
(defpackage :twolang/test/basics
  (:use :cl :fiveam))

(in-package :twolang/test/basics)

(def-suite* basic-test-suite)



(test some-random-test
  (is (string= "hello" "hello")))

(test some-random-test-2
  (is (string= "hello" "hello")))

;; 5am test on C-c C-c config
(setf fiveam:*run-test-when-defined* t)
