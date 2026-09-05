(in-package #:cbor-protocol)

(defclass cbor-serdes-backend (serdes-protocol:serdes-backend) ())

(defun make-cbor-serdes-backend ()
  (make-instance 'cbor-serdes-backend))

(defmethod serdes-protocol:backend-media-type ((backend cbor-serdes-backend))
  "application/cbor")

(defmethod serdes-protocol:backend-binary-p ((backend cbor-serdes-backend))
  t)

(defmethod serdes-protocol:backend-encode ((backend cbor-serdes-backend) value &key stream)
  (declare (ignore backend))
  (encode value :stream stream))

(defmethod serdes-protocol:backend-decode ((backend cbor-serdes-backend) source &key)
  (declare (ignore backend))
  (decode source))

(defclass cbor-binary-input-stream (serdes-protocol:serdes-binary-input-stream)
  ((done :initform nil :accessor cbor-stream-done-p)))

(defclass cbor-binary-output-stream (serdes-protocol:serdes-binary-output-stream) ())

(defmethod serdes-protocol:backend-make-input-stream ((backend cbor-serdes-backend)
                                                      underlying
                                                      &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'cbor-error :message "cbor streams are binary"))
  (make-instance 'cbor-binary-input-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend cbor-serdes-backend)
                                                       underlying
                                                       &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'cbor-error :message "cbor streams are binary"))
  (make-instance 'cbor-binary-output-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:stream-decode-value ((stream cbor-binary-input-stream) &key)
  (if (cbor-stream-done-p stream)
      :eof
      (let* ((in (serdes-protocol:underlying-stream stream))
             (buf (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
             (tmp (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence tmp in)
              do (loop for i from 0 below n do (vector-push-extend (aref tmp i) buf))
              until (< n 4096))
        (when (zerop (length buf))
          (return-from serdes-protocol:stream-decode-value :eof))
        (setf (cbor-stream-done-p stream) t)
        (decode (coerce buf '(vector (unsigned-byte 8)))))))

(defmethod serdes-protocol:stream-encode-value ((stream cbor-binary-output-stream) value &key)
  (write-sequence (encode value) (serdes-protocol:underlying-stream stream))
  value)

(defun use-cbor-serdes-backend ()
  (let ((backend (make-cbor-serdes-backend)))
    (serdes-protocol:register-format :cbor backend
                                     :media-type "application/cbor"
                                     :binary t)
    backend))

(eval-when (:load-toplevel :execute)
  (use-cbor-serdes-backend))
