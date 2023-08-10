(defpackage :twolang/util/env
  (:use :cl)
  (:export
   #:make-env
   #:with-scope
   #:push-scope
   #:pop-scope
   #:add-variable
   #:find-variable
   #:top-level-p
   #:add-variable-toplevel))

(in-package :twolang/util/env)

(defstruct (environment
	    (:constructor %make-env))
  (scope-stack nil))

;; constructor
(defun make-env ()
  (push-scope (%make-env)))

;; scope stack manipulation
(defmacro with-scope (env &body body)
  (let ((res (gensym)))
    `(progn
       (push-scope ,env)
       (let ((,res (progn ,@body)))
	 (pop-scope ,env)
	 ,res))))

(defun push-scope (env)
  (setf (environment-scope-stack env)
	(cons (make-hash-table :test 'equal)
	      (environment-scope-stack env)))
  env)

(defun pop-scope (env)
  (unless (environment-scope-stack env)
    (error "Compiler error: popped empty environment stack"))
  (setf (environment-scope-stack env)
	(cdr (environment-scope-stack env)))
  env)

;; Variable management
(defun add-variable (env symbol &optional (metadata t))
  (setf (gethash symbol
		 (car (environment-scope-stack env)))
	metadata)
  (or metadata symbol))

(defun add-variable-toplevel (env symbol &optional (metadata t))
  (setf (gethash symbol
		 (car (last (environment-scope-stack env))))
	metadata)
  (or metadata symbol))

(defun find-variable (env symbol)
  (find-variable-in-scope-stack (environment-scope-stack env) symbol))

(defun find-variable-in-scope-stack (scope-stack symbol)
  (unless scope-stack
    (return-from find-variable-in-scope-stack nil))
  (multiple-value-bind (value presentp)
      (gethash symbol (car scope-stack))
    (if presentp
	value
	(find-variable-in-scope-stack (cdr scope-stack) symbol))))

;; other utils
(defun env-depth (env)
  (length (environment-scope-stack env)))

(defun top-level-p (env)
  (eq (env-depth env) 1))

