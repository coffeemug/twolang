(defpackage :twolang/cmd/repl
  (:use :cl)
  (:import-from :clingon)
  (:export
   #:make-repl-command))

(in-package :twolang/cmd/repl)

;; clingon command stuff
(defun make-repl-command ()
  (clingon:make-command
   :name "repl"
   :description "start twolang repl"
   :handler #'repl-command-handler))

(defun repl-command-handler (cmd)
  (declare (ignore cmd)))

