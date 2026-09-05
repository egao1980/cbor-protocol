(defpackage #:cbor-protocol
  (:use #:cl)
  (:nicknames #:stack-cbor)
  (:export #:cbor-error
           #:cbor-encode-error
           #:cbor-parse-error
           #:cbor-error-message

           #:cbor-tag
           #:cbor-tag-p
           #:cbor-tag-number
           #:cbor-tag-value
           #:make-cbor-tag

           #:*cbor-backend*
           #:cbor-backend
           #:backend-encode
           #:backend-decode
           #:make-cbor-backend
           #:use-cbor-backend

           #:encode
           #:decode
           #:encode-to-octets
           #:decode-octets

           #:null-p
           #:true-p
           #:false-p

           #:cbor-serdes-backend
           #:make-cbor-serdes-backend
           #:use-cbor-serdes-backend
           #:install-http-cbor-hooks))

(in-package #:cbor-protocol)
