(defpackage :twolang/lexed-input
  (:use :cl :maxpc.input)
  (:export
   #:lexed-input #:make-lexed-input #:source-input #:active-lexer #:input-sequence))

(in-package :twolang/lexed-input)

(defclass lexed-input ()
  ((source-input :initarg :source-input :accessor source-input)
   (active-lexer :initarg :active-lexer :accessor active-lexer)
   (pos :initarg :input-position :accessor lexed-input-pos)
   (cached-element :accessor cached-element :initform nil)))

(defun make-lexed-input (source-input initial-lexer)
  (make-instance 'lexed-input
		 :source-input (make-input source-input)
		 :active-lexer initial-lexer
		 :input-position 0))

(defmethod input-empty-p ((obj lexed-input))
  (cond
    ((cached-element obj) nil)
    (t (or 
	(null (source-input obj))
	(input-empty-p (source-input obj))))))

(defmethod input-first ((obj lexed-input))
  (unless (cached-element obj)
    (multiple-value-bind (rest result resultp)
	(funcall (active-lexer obj) (source-input obj))
      (when (not rest)
	(error "Failed to match, unexpected symbol `~a`" (input-first (source-input obj))))
      (setf (source-input obj) rest)
      (when (not resultp)
	;; if lexer returns an empty token (i.e. on whitespace), we consume again
	(return-from input-first (input-first obj)))
      (setf (cached-element obj)
	    (list rest result resultp))))
  (destructuring-bind (rest result resultp)
      (cached-element obj)
    (declare (ignore rest resultp))
    result))

(defmethod input-rest ((obj lexed-input))
  (input-first obj)
  (make-instance 'lexed-input
		 :source-input (source-input obj)
		 :active-lexer (active-lexer obj)
		 :input-position (1+ (lexed-input-pos obj))))

(defmethod make-input ((obj lexed-input))
  obj)

(defmethod input-position ((obj lexed-input))
  (lexed-input-pos obj))
