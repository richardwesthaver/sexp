;;; sxp.lisp --- S-eXPressions

;; sxp is a unified S-Expression data format

;;; Code:
(defpackage :sxp
  (:use :cl :sb-mop :sym :fu)
  (:import-from :uiop :slurp-stream-forms :read-file-forms :with-output-file)
  ;; TODO: hot-patch readtables into sxp classes/parsers
  (:import-from :macs.readtables :defreadtable :in-readtable)
  (:export
   :form :sxp-error :sxp-fmt-error :sxp-syntax-error :reader :writer :fmt
   :wrap :unwrap :unwrap! :unwrap-or
   :sxpp :build-ast :load-ast
   :define-macro :define-fmt :read-sxp-file :write-sxp-file
   :read-sxp-string :write-sxp-string :read-sxp-stream :write-sxp-stream
   :make-sxp :sxp :formp :form
   :list-slot-values-using-class :list-class-methods :list-class-slots :list-indirect-slot-methods
   :wrap-object :unwrap-object))

(in-package :sxp)

;;; Utils
(deftype form ()
  '(satisfies formp))

(define-condition sxp-error (error) ())

(define-condition sxp-fmt-error (sxp-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
             (apply 'format s (format-control c) (format-arguments c)))))

(define-condition sxp-syntax-error (sxp-error) ())

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

;;; Prototypes
(defstruct sxp-object-prototype
  )
(make-sxp-object-prototype)
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

(defun list-indirect-class-methods (class)
  "List all indirect methods of CLASS."
  (remove-duplicates (mapcan #'specializer-direct-generic-functions (compute-class-precedence-list class))))

(defun list-class-methods (class methods &optional indirect)
  "List all methods specializing on CLASS modulo METHODS. When INDIRECT is
non-nil, also include indirect (parent) methods."
  (if (eq methods t)
      (if indirect
	  (list-indirect-class-methods class)
	  (specializer-direct-generic-functions class))
      (mapcar
       (lambda (s)
	 (car (member s (specializer-direct-generic-functions class) :key #'generic-function-name)))
       methods)))

;; TODO 2023-09-09: slot exclusion from dynamic var
(defun list-class-slots (class slots)
  ;; should probably convert slot-definition-name here
  (let ((cs (remove-if
	     (lambda (s) (or
			  (null s)
			  (eq (slot-definition-name s) 'sxp::ast)))
	     (class-slots class))))
    (if (eq slots t)
	cs
	(loop for s in slots
	      with sn = (symb s)
	      for c in cs
	      with cn = (symb (slot-definition-name c))
	      when (eq sn cn)
		collect c))))

(defun list-slot-values-using-class (class obj slots &optional nullp unboundp)
  (remove-if
   #'null
   (mapcar
    (lambda (s)
      (let ((n (slot-definition-name s)))
	(let ((ns (make-keyword (symbol-name n))))
	  (if (slot-boundp-using-class class obj s)
	      (let ((v (slot-value-using-class class obj s)))
		(if nullp
		    (cons ns v)
		    (unless (null v)
		      (cons ns v))))
	      (when unboundp (list ns))))))
    slots)))

(defun unwrap-object (obj &key (slots t) (methods nil)
			    (indirect nil) (tag nil)
			    (unboundp nil) (nullp nil))
  "Build and return a new `form' from OBJ by traversing the class
definition. This differs from the generic function `unwrap' which
always uses the ast slot as an internal buffer. We can also call this
on any class instance (doesn't need to subclass `sxp').

SLOTS specifies the slots to be included in the output. If the value
is t, all slots are included. The ast slot is not included by default,
but this behavior may change in future revisions.

When INDIRECT is non-nil, also include methods which indirectly
specialize on OBJ.

When TAG is non-nil, return a cons where car is TAG and cdr is the
output. If TAG is t, use the class-name symbol."
  (declare (type standard-object obj)
	   (type (or list boolean) slots)
	   (type (or list boolean) methods)
	   (type boolean indirect)
	   (ignorable indirect))
  (unless (or slots methods)
    (error "Required one missing key arg: SLOTS or METHODS"))
  (let* ((class (class-of obj))
	(res (when tag (list (if (eq t tag) (class-name class) tag)))))
    (block unwrap
      (when-let ((slots (when slots
			  (list-class-slots class slots))))
	(let ((slot-vals (list-slot-values-using-class class obj (remove-if #'null slots) nullp unboundp)))
	  (if methods
	      (push slot-vals res)
	      (return-from unwrap (push slot-vals res)))))
      (when-let ((methods (when methods (list-class-methods class methods indirect))))
	(push methods res)))
    (flatten (nreverse res))))

(defun wrap-object (class form)
  "Given a CLASS prototype and an input FORM, return a new instance of
CLASS. FORM is assumed to be the finalized lisp object which has
already passed through `read' -- not a string or file-stream for
example."
  (declare (type class class)
	   (type form form))
	)

;; (defmacro define-fmt ())
;; (defmacro define-macro ())
