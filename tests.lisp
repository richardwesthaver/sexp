;;; tests.lisp --- SEXP tests
(defpackage :sxp-tests
  (:use :cl :sxp :fiveam)
  (:shadowing-import-from #:fiveam #:test)
  (:export #:run-tests))
(in-package :sxp-tests)
(defparameter *test-file* #p"tests.sxp")
(def-suite :sxp)
(in-suite :sxp)
(test sxp-file
  (let ((f (read-sxp-file *test-file*)))
    (is (equal (sxp:unwrap f) (sxp:unwrap (read-sxp-stream (open *test-file*)))))))

(defun run-tests ()
  (run! :sxp))
