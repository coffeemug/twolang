(defpackage :twolang/cmd/repl
  (:use :cl :twolang/util/ast :twolang/lex :twolang/parse :twolang/tc :twolang/cc)
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

(defstruct repl-opts
  (print-lex-p nil)
  (print-ast-p nil)
  (print-lisp-p nil))

(defun repl-command-handler (cmd)
  (declare (ignore cmd))
  (print-platform-header)
  (maybe-start-slynk)
  (let ((*repl-ctrl* (make-repl-opts)))
    (declare (special *repl-ctrl*))
    (repl)))

(defun print-platform-header ()
  (let ((mt (machine-type))
	(mv (machine-version))
	(st (software-type))
	(sv (software-version))
	(lt (lisp-implementation-type))
	(lv (lisp-implementation-version)))
    (with-color (:black :effect :bright)
      (format t #?"[${mt}/${mv}] [${st}/${sv}] [${lt}/${lv}] [Twolang/0.1]~%"))
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
  (handler-bind ((error 'on-error))
    (restart-case (or
		   (maybe-eval-repl-command line)
		   (eval-line* line))
      (skip-expression () nil))))

(defun eval-line* (line)
  (declare (special *repl-ctrl*))
  (let ((lexed (lex line)))
    (when (repl-opts-print-lex-p *repl-ctrl*)
      (format t "~s~%" lexed))
    (let ((parsed (parse lexed)))
      (when (repl-opts-print-ast-p *repl-ctrl*)
	(format t "~s~%" parsed))
      (let* ((checked (tc! (parse (lex line))))
	     (compiled (cc checked)))
	(when (repl-opts-print-lisp-p *repl-ctrl*)
	  (format t "~s~%" compiled))
	(with-color (:black :effect :bright)
	  (format t "=> "))
	(format t "~s :: ~a~%~%"
	    (funcall (compile nil `(lambda () ,compiled)))
	    (type-hrepr (node-type checked)))))))

(defun on-error (error)
  (if *debugger-hook*
      (invoke-debugger error)
      (progn
	(format t "Error: ~a~%~%" error)
	(invoke-restart (find-restart 'skip-expression)))))

(defun maybe-eval-repl-command (line)
  (declare (special *repl-ctrl*))
  (let ((line (string-trim '(#\Space #\Tab #\Newline) line)))
    (when (equal (uiop:first-char line) #\!)
      (cond
	((equal line "!lex+") (setf (repl-opts-print-lex-p *repl-ctrl*) t))
	((equal line "!lex-") (setf (repl-opts-print-lex-p *repl-ctrl*) nil))
	((equal line "!ast+") (setf (repl-opts-print-ast-p *repl-ctrl*) t))
	((equal line "!ast-") (setf (repl-opts-print-ast-p *repl-ctrl*) nil))
	((equal line "!lisp+") (setf (repl-opts-print-lisp-p *repl-ctrl*) t))
	((equal line "!lisp-") (setf (repl-opts-print-lisp-p *repl-ctrl*) nil))
	((equal (uiop:first-char line) #\!) (format t "[err] unknown command~%")))
      (print-repl-ctrl)
      t)))

(defun print-repl-ctrl ()
  (declare (special *repl-ctrl*))
  (let ((lexp (repl-opts-print-lex-p *repl-ctrl*))
	(astp (repl-opts-print-ast-p *repl-ctrl*))
	(lispp (repl-opts-print-lisp-p *repl-ctrl*))))
  (with-color (:black :effect :bright)
    (format t "[lex~a] " (if (repl-opts-print-lex-p *repl-ctrl*) #\+ #\-))
    (format t "[ast~a] " (if (repl-opts-print-ast-p *repl-ctrl*) #\+ #\-))
    (format t "[lisp~a] " (if (repl-opts-print-lisp-p *repl-ctrl*) #\+ #\-))
    (format t "~%~%"))
  (finish-output))
