;;; tests.lisp --- SEXP tests
(defpackage :sxp-tests
  (:use :cl :sxp :fiveam)
  (:shadowing-import-from #:fiveam #:test)
  (:export #:run-tests))

(in-package :sxp-tests)

(def-suite :sxp)

(defun run-tests ()
  (run! :sxp))
