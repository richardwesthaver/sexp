(defpackage :sxp-bench
  (:use :cl :sxp :sb-unix :sb-ext)
  (:export :run-bench :*bench-input-file* :*bench-input-string* :*bench-input-object*
	   :*bench-output-directory* :*bench-iterations* :*bench-report-file*))
(in-package :sxp-bench)

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

(defun run-bench (&optional report)
  (when (probe-file *bench-output-directory*)
    (sb-ext:delete-directory *bench-output-directory* :recursive t))
  (sb-unix:unix-mkdir *bench-output-directory* #o777)
  (when report
    (format t "Writing output to ~s" *bench-report-file*)
;;    (setq report (open *bench-report-file* :direction :output :if-exists :overwrite :if-does-not-exist :create)))
  (let ((rres (rbench #'sxp:read-sxp-file *bench-input-file*))
	(wres (wbench #'sxp:write-sxp-file)))
    (uiop:with-output-file (out *bench-report-file* :if-exists :supersede :if-does-not-exist :create)
      (print (list rres wres) out)))))
