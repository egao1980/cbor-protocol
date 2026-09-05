(in-package #:cbor-protocol)

(define-condition cbor-error (error)
  ((message :initarg :message :reader cbor-error-message :initform nil))
  (:report (lambda (c s)
             (format s "CBOR error~@[: ~a~]" (cbor-error-message c)))))

(define-condition cbor-encode-error (cbor-error) ())

(define-condition cbor-parse-error (cbor-error) ())
