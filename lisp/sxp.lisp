;;; sexp.lisp --- SEXP
(defpackage :sxp
  (:use :cl))

(in-package :sxp)

(defvar read-table (make-hash-table))
(defvar write-table (make-hash-table))

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
