(defsystem "cbor-protocol"
  :version "0.1.0"
  :description "CLOS CBOR encode/decode for cl-stack (RFC 8949); same Lisp mapping as json-protocol; implements serdes-protocol :cbor"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
  :properties (:cl-repo (:ci (:sources (("serdes-protocol" :oci)))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "codec")
               (:file "protocol")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "cbor-protocol/tests"))))

(defsystem "cbor-protocol/tests"
  :depends-on ("cbor-protocol" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "codec-test")
               (:file "serdes-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
