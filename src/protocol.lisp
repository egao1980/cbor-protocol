(in-package #:cbor-protocol)

(defvar *cbor-backend* nil
  "Current CBOR backend object.")

(defclass cbor-backend () ()
  (:documentation "Base class for cbor-protocol backends."))

(defgeneric backend-encode (backend value &key stream)
  (:documentation "Encode VALUE as CBOR octets, or write STREAM."))

(defgeneric backend-decode (backend source &key)
  (:documentation "Decode SOURCE (octets, stream, or hex-incompatible string)."))

(defun null-p (object)
  (eq object :null))

(defun true-p (object)
  (eq object t))

(defun false-p (object)
  (and (null object) (not (eq object :null))))

(defun %source-octets (source)
  (etypecase source
    ((vector (unsigned-byte 8)) source)
    (vector
     (if (and (not (stringp source))
              (every (lambda (b) (typep b '(unsigned-byte 8))) source))
         (coerce source '(vector (unsigned-byte 8)))
         (error 'cbor-parse-error :message "CBOR decode needs octets")))
    (stream
     (let ((buf (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
           (tmp (make-array 4096 :element-type '(unsigned-byte 8))))
       (loop for n = (read-sequence tmp source)
             do (loop for i from 0 below n do (vector-push-extend (aref tmp i) buf))
             until (< n 4096))
       (coerce buf '(vector (unsigned-byte 8)))))))

(defclass native-cbor-backend (cbor-backend) ())

(defun make-cbor-backend ()
  (make-instance 'native-cbor-backend))

(defmethod backend-encode ((backend native-cbor-backend) value &key stream)
  (declare (ignore backend))
  (let ((octets (encode-cbor value)))
    (if stream
        (progn (write-sequence octets stream) (values))
        octets)))

(defmethod backend-decode ((backend native-cbor-backend) source &key)
  (declare (ignore backend))
  (decode-cbor (%source-octets source)))

(defun use-cbor-backend ()
  (setf *cbor-backend* (make-cbor-backend)))

(defun encode (value &key stream)
  (unless *cbor-backend*
    (error 'cbor-encode-error :message "*cbor-backend* is unbound — load cbor-protocol"))
  (backend-encode *cbor-backend* value :stream stream))

(defun decode (source &key)
  (unless *cbor-backend*
    (error 'cbor-parse-error :message "*cbor-backend* is unbound — load cbor-protocol"))
  (backend-decode *cbor-backend* source))

(defun encode-to-octets (value &key)
  (encode value))

(defun decode-octets (octets &key)
  (decode octets))

(defun install-http-cbor-hooks ()
  "If http-protocol is loaded, register :cbor data (de)serializers."
  (let ((pkg (find-package :http-protocol)))
    (unless pkg
      (return-from install-http-cbor-hooks nil))
    (flet ((push-codec (table-sym type fn)
             (let ((s (find-symbol table-sym pkg)))
               (when (and s (boundp s))
                 (setf (symbol-value s)
                       (acons type fn (remove type (symbol-value s) :key #'car)))))))
      (push-codec "*DATA-SERIALIZERS*" :cbor #'encode)
      (push-codec "*DATA-DESERIALIZERS*" :cbor #'decode))
    t))

(eval-when (:load-toplevel :execute)
  (use-cbor-backend)
  (install-http-cbor-hooks))
