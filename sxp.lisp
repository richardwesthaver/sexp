;;; sxp.lisp --- S-eXPressions
(defpackage :sxp
  (:use :cl)
  (:import-from :uiop :slurp-stream-forms :read-file-forms :with-output-file)
  (:import-from :named-readtables :defreadtable :in-readtable)
  (:export :form :reader :writer :fmt :wrap :unwrap
	   :define-macro :define-fmt :read-sxp-file :write-sxp-file
	   :read-sxp-string :write-sxp-string :read-sxp-stream :write-sxp-stream
	   :make-sxp :sxp :formp :form))
(in-package :sxp)

(defun formp (form)
  (or (consp form) (atom form)))

(deftype form ()
  '(satisfies formp))

(defgeneric wrap (sxp form))
(defgeneric unwrap (sxp))

(defclass sxp ()
  ;; will eventually not contain slots, abstract only class
  ;; implemented for objects.
  ((ast :initarg :ast :type form))
  (:documentation "loosely typed object representing a SXP form."))

(defmethod wrap ((self sxp) form) (setf (slot-value self 'ast) form))
(defmethod unwrap ((self sxp)) (slot-value self 'ast))

;; (defsetf unwrap ) (defsetf wrap )

(defun read-sxp-file (file)
  (make-instance 'sxp :ast (read-file-forms file)))
(defun write-sxp-file (sxp file &optional &key if-exists)
  (with-output-file (out file) :if-exists if-exists
    (print (slot-value sxp 'ast) out)))

(defun read-sxp-stream (stream) (make-instance 'sxp :ast (slurp-stream-forms stream :count nil)))
(defun write-sxp-stream (sxp stream) (print (slot-value sxp 'ast) stream))

(defun read-sxp-string (str) (with-input-from-string (s str) (read-sxp-stream s)))
(defun write-sxp-string (sxp) (write-to-string (slot-value sxp 'ast)))

(defmacro make-sxp (&optional &rest form) `(make-instance 'sxp ,@(when form `(:ast ',form))))
;; (defmacro define-fmt ())
;; (defmacro define-macro ())
