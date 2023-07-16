;;; sexp.lisp --- SEXP
(defpackage :sxp
  (:use :cl :alexandria)
  (:export :read-table :write-table :form :reader :writer :fmt))

(in-package :sxp)

(defvar read-table (make-hash-table))
(defvar write-table (make-hash-table))

(defgeneric read-object ())
(defgeneric write-object ())
(defgeneric read-value ())
(defgeneric write-value ())
(defgeneric read-list ())
(defgeneric write-list ())
(defgeneric read-plist ())
(defgeneric write-plist ())

(defclass form ()
  ()
  (:documentation "loosely typed object representing a SXP form."))

(defclass reader ()
  ()
  (:documentation "Reader object which returns raw SXP forms from a stream."))

(defclass writer ()
  ()
  (:documentation "Writer object which writes a raw SXP to a stream."))

(defclass fmt ()
  ()
  (:documentation "Formatter object which dictates how a SXP form is to be encoded."))

(defmacro define-fmt ())

(defmacro define-macro ())
