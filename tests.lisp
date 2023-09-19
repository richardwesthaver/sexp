;;; tests.lisp --- SEXP tests
;; TODO 2023-09-01: refactor to macs.rt
(defpackage :sxp-tests
  (:use :cl :sxp :rt)
  (:export #:run-tests))
(in-package :sxp-tests)

(declaim
 (type (or string pathname) *test-file*)
 (type string *test-string*))
(defparameter *test-file* #p"tests.sxp")
(defparameter *test-string* "(foo 'bar `(\"test\" ,baz ,@qux) 123 0.0123 1/3 `(,a1 ,a2))")

(defsuite :sxp)
(in-suite :sxp)

(deftest forms ()
  (is (formp nil))
  (is (formp t)))

(deftest sxp-file ()
  (let ((f (read-sxp-file *test-file*)))
    (is (equal (unwrap f) (unwrap f)))))

(deftest sxp-string ()
  (let ((f (make-instance 'sxp)))
    (is (formp (read-sxp-string f *test-string*)))
    (is (stringp (write-sxp-string f)))))

(deftest sxp-stream ()
  (let ((f (make-instance 'sxp)))
    (with-input-from-string (s *test-string*)
      (read-sxp-stream f s))
    (is (write-sxp-stream f nil))))
