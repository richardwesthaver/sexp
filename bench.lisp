(defpackage :sxp-bench
  (:use :cl :sxp :sb-ext :sb-unix)
  (:export :run-bench :*bench-input-file* :*bench-input-string* :*bench-input-object*
	   :*bench-output-directory* :*bench-iterations* :*bench-report-file*))
(in-package :sxp-bench)
(require :sb-sprof)
(declaim
 (type (or string pathname) *bench-input-file* *bench-output-directory* *bench-report-file*)
 (type string *bench-input-string*)
 (type sxp *bench-input-object*)
 (type integer *bench-iterations*))
(defparameter *bench-input-file* "tests.sxp")
(defparameter *bench-input-string* (uiop:read-file-string *bench-input-file*))
(defparameter *bench-input-object* (sxp:read-sxp-string *bench-input-string*))
(defparameter *bench-output-directory* "/tmp/sxp-bench")
(defparameter *bench-iterations* 1000)
(defparameter *bench-report-file* "report.sxp")

(defmacro bench (&body body)
  `(loop for i from 1 to *bench-iterations*
	 do ,@body))

(defun rbench (fn input)
  (let ((res))
    (bench (call-with-timing (lambda (&rest x) (push (cons i x) res)) fn input))
    (nreverse res)))

(defun wbench (fn)
  (let ((res))
    (bench (let ((out (make-pathname :name (format nil "~d.sxp" i) :directory *bench-output-directory*)))
	     (call-with-timing (lambda (&rest x) (push (cons i x) res)) fn *bench-input-object* out)))
    (nreverse res)))

(defun run-bench (&optional rpt)
  (when (probe-file *bench-output-directory*)
    (sb-ext:delete-directory *bench-output-directory* :recursive t))
  (sb-unix:unix-mkdir *bench-output-directory* #o777)
  (let ((rres (sb-sprof:with-profiling (:sample-interval 0.001) (rbench #'sxp:read-sxp-file *bench-input-file*)))
	      (wres (sb-sprof:with-profiling (:sample-interval 0.001) (wbench #'sxp:write-sxp-file) )))
    (if rpt
	(progn
	  (format t "Writing output to ~s" *bench-report-file*)
	  (uiop:with-output-file (out *bench-report-file* :if-exists :supersede :if-does-not-exist :create)
	    (print (list rres wres) out)))
	(print (list rres wres))))
  (terpri))
