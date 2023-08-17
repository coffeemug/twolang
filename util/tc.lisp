(defpackage :twolang/util/tc
  (:use :cl :twolang/util/env)
  (:export
   #:with-tc #:*tc-env*))

(in-package :twolang/util/tc)

(defmacro with-tc (&body body)
  `(let ((*tc-env* (make-env)))
     (declare (special *tc-env*))
     ,@body))

