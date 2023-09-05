;;; tests.lisp --- SEXP tests
;; TODO 2023-09-01: refactor to macs.rt
(defpackage :sxp-tests
  (:use :cl :sxp :fiveam)
  (:export #:run-tests))
(in-package :sxp-tests)

(declaim
 (type (or string pathname) *test-file*)
 (type string *test-string*))
(defparameter *test-file* #p"tests.sxp")
(defparameter *test-string* "(foo 'bar `(\"test\" ,baz ,@qux) 123 0.0123 1/3 `(,a1 ,a2))")

(def-suite :sxp)
(in-suite :sxp)

(test forms
  (is (formp nil))
  (is (typep '(1 2 3) 'form)))

(test sxp-file
  (let ((f (read-sxp-file *test-file*)))
    (is (equal (unwrap f) (unwrap (read-sxp-stream (open *test-file*)))))))
(test sxp-string
  (is (stringp (write-sxp-string (read-sxp-string *test-string*)))))
(test sxp-stream
  (is (write-sxp-stream (with-input-from-string (s *test-string*) (read-sxp-stream s)) nil)))

(defun run-tests ()
  (run! :sxp))
