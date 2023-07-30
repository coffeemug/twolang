(defpackage :twolang/cmd/repl
  (:use :cl :twolang/lex :twolang/parse :twolang/tc)
  (:import-from :clingon)
  (:import-from :cl-interpol)
  (:import-from :cl-readline)
  (:import-from :cl-ansi-text :with-color)
  (:export
   #:make-repl-command))

(in-package :twolang/cmd/repl)

(named-readtables:in-readtable :interpol-syntax)

;; clingon command stuff
(defun make-repl-command ()
  (clingon:make-command
   :name "repl"
   :description "start twolang repl"
   :handler #'repl-command-handler))

(defun repl-command-handler (cmd)
  (declare (ignore cmd))
  (print-platform-header)
  (maybe-start-slynk)
  (repl))

(defun print-platform-header ()
  (let ((mt (machine-type))
	(mv (machine-version))
	(st (software-type))
	(sv (software-version))
	(lt (lisp-implementation-type))
	(lv (lisp-implementation-version)))
    (with-color (:black :effect :bright)
      (format t #?"[${mt}/${mv}] [${st}/${sv}] [${lt}/${lv}] [Onelang/0.1]~%"))
    (finish-output)))

(defun maybe-start-slynk ()
  "Dynamically load and start slynk server if the `slynk` system is
installed."
  (when (asdf:find-system :slynk nil)
    (let* ((null-stream (make-two-way-stream
			 (make-concatenated-stream)
			 (make-broadcast-stream)))
	   (*debug-io* null-stream))
      (asdf:load-system :slynk))
    (funcall
     (find-symbol
      (string '#:create-server)
      (find-package :slynk)))))

(defun repl ()
  (let ((count 0))
    (loop for line = (progn
		       (finish-output)
		       (readline (make-prompt count)))
	  while line do (when (not (string= line ""))
			  (eval-line line)
			  (incf count)))))

(defun readline (prompt)
  (rl:readline :prompt prompt :add-history t))

(defun make-prompt (count)
  (with-output-to-string (stream)
    (with-color (:green :stream stream)
      (format stream "[" count))
    (with-color (:green :effect :bright :stream stream)
      (format stream "~d" count))
    (with-color (:green :stream stream)
      (format stream "]: " count))))

(defun eval-line (line)
  (format t "~s~%~%" (tc! (parse (lex line)))))
