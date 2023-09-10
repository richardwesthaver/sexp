;;; sxp.lisp --- S-eXPressions

;; sxp is a unified S-Expression data format

;;; Code:
(defpackage :sxp
  (:use :cl :sb-mop)
  (:import-from :uiop :slurp-stream-forms :read-file-forms :with-output-file)
  ;; TODO: hot-patch readtables into sxp classes/parsers
  (:import-from :macs.readtables :defreadtable :in-readtable)
  (:export :form :reader :writer :fmt :wrap :unwrap :unwrap! :unwrap-or :sxpp
	   :build-ast :load-ast
	   :define-macro :define-fmt :read-sxp-file :write-sxp-file
	   :read-sxp-string :write-sxp-string :read-sxp-stream :write-sxp-stream
	   :make-sxp :sxp :formp :form :wrap-object :unwrap-object))
(in-package :sxp)

;;; Utils
(defun formp (form)
  (or (consp form) (atom form)))

(deftype form ()
  '(satisfies formp))

 ;;; Protocol
(defgeneric wrap (self form))
(defgeneric unwrap (self))
(defgeneric unwrap! (self))
(defgeneric unwrap-or (self lambda))
(defgeneric sxpp (self form))

(defgeneric build-ast (self)
  (:documentation "build the sxp representation of SELF and store it in the :ast
slot. The :ast slot is always ignored."))

(defgeneric load-ast (self)
  (:documentation "load the object SELF from the :ast slot."))

;;; Objects
(defclass sxp ()
  ((ast :initarg :ast :type form))
  (:documentation "Dynamic class representing a SXP form."))

(defmethod wrap ((self sxp) form) (setf (slot-value self 'ast) form))

(defmethod unwrap ((self sxp)) (slot-value self 'ast))

(defmethod unwrap! ((self sxp)) (ignore-errors (slot-value self 'ast)))

(defmethod unwrap-or ((self sxp) else-fn)
  (if (slot-unbound 'sxp self 'ast)
      (slot-value self 'ast)
      (if (null (slot-value self 'ast))
	  (funcall else-fn))))

;; (defsetf unwrap ) (defsetf wrap )

;;; Functions
(defun read-sxp-stream (stream)
  (make-instance 'sxp :ast (slurp-stream-forms stream :count nil)))

(defun read-sxp-file (file)
  (make-instance 'sxp :ast (read-file-forms file)))

(defun write-sxp-stream (sxp stream &key (pretty *print-pretty*) (case :downcase))
  (write (slot-value sxp 'ast)
	 :stream stream
	 :pretty pretty
	 :case case))

(defun write-sxp-file (sxp file &optional &key if-exists)
  (with-output-file (out file) :if-exists if-exists
    (write-sxp-stream (slot-value sxp 'ast) out)))

(defun read-sxp-string (str) (with-input-from-string (s str) (read-sxp-stream s)))

(defun write-sxp-string (sxp) (write-to-string (slot-value sxp 'ast)))

(defmacro make-sxp (&optional &rest form) `(make-instance 'sxp ,@(when form `(:ast ',form))))

(defun list-indirect-methods (class)
  "List all indirect methods of CLASS."
  (remove-duplicates (mapcan #'specializer-direct-generic-functions (compute-class-precedence-list class))))

(defun list-methods (class methods &optional indirect)
  "List all methods specializing on CLASS modulo METHODS. When INDIRECT is
non-nil, also include indirect (parent) methods."
  (when methods
    (if (eq methods t)
	(if indirect
	    (list-indirect-methods class)
	    (specializer-direct-generic-functions class))
	(mapcar
	 (lambda (s)
	   (car (member s (specializer-direct-generic-functions class) :key #'generic-function-name)))
	 methods))))

;; TODO 2023-09-09: slot exclusion from dynamic var
(defun list-slots (class slots)
  (let ((cs (remove-if (lambda (s) (eq (slot-definition-name s) 'ast)) (class-slots class))))
    (if (eq slots t)
	cs
	(mapcar
	 (lambda (s) (car (member s cs :key #'slot-definition-name)))
	 slots))))
    
(defun unwrap-object (obj &key (slots t) (methods t) (indirect nil) (tag nil))
  "Build and return a form from OBJ.

SLOTS specifies the slots to be included in the output. If the value
is t, all slots are included.

When INDIRECT is non-nil, also include methods which indirectly
specialize on OBJ.

When TAG is non-nil, return a cons where car is TAG and cdr is the
output. If TAG is t, use the class-name symbol."
  (declare (type standard-object obj)
	   (type (or list boolean) slots)
	   (type (or list boolean) methods)
	   (type boolean indirect)
	   (ignorable indirect))
  (let* ((class (class-of obj)) 
	 (slots (list-slots class slots))
	 (methods (list-methods class methods indirect))
	 (res)) ;; result var
    (setf res class)
    (when slots (setf res (cons res slots)))
    (when methods (setf res (cons res methods)))
    (when tag (setf res (if (eq t tag) (cons (class-name class) res) (cons tag res))))
    res))

(defun wrap-object (class form))

;; (defmacro define-fmt ())
;; (defmacro define-macro ())
