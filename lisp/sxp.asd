;;; sxp.asd
(defsystem "sxp"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description ""
  :homepage "https://rwest.io/sxp"
  :bug-tracker "https://lab.rwest.io/comp/sxp/issues"
  :source-control (:hg "https://lab.rwest.io/comp/sxp")
  :license "WTF"
  :depends-on ("alexandria")
  :in-order-to ((test-op (test-op "tests")))
  :build-pathname "sxp"
  :components ((:file "sxp")))

(defmethod perform :after ((op load-op) (c (eql (find-system :sxp))))
  (pushnew :sxp *features*))

(defsystem "sxp/tests"
  :depends-on ("sxp" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (op co)
		    (uiop:symbol-call '#:sxp-tests '#:run-tests)))
