;;; sxp.asd
(defsystem "sxp"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :depends-on ("named-readtables")
  :description ""
  :homepage "https://rwest.io/sxp"
  :bug-tracker "https://lab.rwest.io/comp/sxp/issues"
  :source-control "https://lab.rwest.io/comp/sxp"
  :license "WTF"
  :in-order-to ((test-op (test-op :sxp/tests)))
  :build-pathname "sxp"
  :components ((:file "sxp")))

(defmethod perform :after ((op load-op) (c (eql (find-system :sxp))))
  (pushnew :sxp *features*))

(defsystem "sxp/tests"
  :depends-on ("sxp" "fiveam" "uiop")
  :components ((:file "tests"))
  :perform (test-op (op c)
		    (uiop:symbol-call '#:sxp-tests '#:run-tests)))

(defsystem "sxp/bench"
  :depends-on ("sxp" "uiop" "sb-sprof")
  :components ((:file "bench"))
  :perform (test-op (op c) (uiop:symbol-call '#:sxp-bench '#:run-bench)))
