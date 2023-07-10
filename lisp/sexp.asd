;;; sexp.asd
(defsystem "sexp"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description ""
  :homepage "https://rwest.io/sexp"
  :bug-tracker "https://lab.rwest.io/comp/sexp/issues"
  :source-control (:hg "https://lab.rwest.io/comp/sexp")
  :license "WTF"
  :in-order-to ((test-op (test-op "tests")))
  :build-pathname "sexp"
  :components ((:file "sexp")))

(defmethod perform :after ((op load-op) (c (eql (find-system :sexp))))
  (pushnew :sexp *features*))

(defsystem "sexp/tests"
  :depends-on ("sexp" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (op co)
		    (uiop:symbol-call '#:sexp-tests '#:run-tests)))
