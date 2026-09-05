(in-package #:cbor-protocol/tests)

(deftest serdes-cbor
  (let ((octets (serdes-protocol:encode 42 :format :cbor)))
    (ok (equalp (encode 42) octets))
    (ok (= 42 (serdes-protocol:decode octets :format :cbor)))
    (ok (serdes-protocol:format-binary-p :cbor))
    (ok (string= "application/cbor" (serdes-protocol:format-media-type :cbor)))
    (ok (eq :cbor (serdes-protocol:find-format-for-media-type "application/cbor")))))
