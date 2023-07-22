;;; sxp.lisp --- S-eXPressions
(defpackage :sxp
  (:use :cl)
  (:import-from :uiop :slurp-stream-forms)
  (:export :read-table :write-table :form :reader :writer :fmt :wrap :unwrap
	   :define-macro :define-fmt :read-sxp-file :write-sxp-file
	   :read-sxp-string :write-sxp-string :read-sxp-stream :write-sxp-stream
	   :make-sxp :sxp))
(in-package :sxp)

(defvar read-table (make-hash-table))
(defvar write-table (make-hash-table))

(defgeneric wrap (sxp form))
(defgeneric unwrap (sxp))

(defclass sxp ()
  ((ast :initarg :ast))
  (:documentation "loosely typed object representing a SXP form."))
(defmethod wrap ((self sxp) form) (setf (slot-value self 'ast) form))
(defmethod unwrap ((self sxp)) (slot-value self 'ast))

;; (defsetf unwrap ) (defsetf wrap )

(defun read-sxp-file (file)
  (make-instance 'sxp :ast (uiop:read-file-forms file)))
(defun write-sxp-file (sxp file &optional &key if-exists)
  (uiop:with-output-file (out file) :if-exists if-exists
    (print (slot-value sxp 'ast) out)))
(defun read-sxp-stream (stream) (make-instance 'sxp :ast (uiop:slurp-stream-forms stream :count nil)))
(defun write-sxp-stream (sxp stream) (print (slot-value sxp 'ast) stream))
(defun read-sxp-string (str) (with-input-from-string (s str) (read-sxp-stream s)))
(defun write-sxp-string (sxp) (write-to-string (slot-value sxp 'ast)))
(defun make-sxp () (make-instance 'sxp))
;; (defmacro define-fmt ())
;; (defmacro define-macro ())
