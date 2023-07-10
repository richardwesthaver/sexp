;;; tests.lisp --- SEXP tests
(defpackage :sexp-tests
  (:use :cl :sexp :fiveam)
  (:shadowing-import-from #:fiveam #:test)
  (:export #:run-tests))

(in-package :sexp-tests)

(def-suite :sexp)

(defun run-tests ()
  (run! :sexp))
