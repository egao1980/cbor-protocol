(in-package #:cbor-protocol)

;;; RFC 8949 definite encoding. Decode accepts indefinite lengths.
;;; Lisp mapping matches json-protocol, plus bytes and tags:
;;;   map → equal hash-table (string keys when the CBOR key is text)
;;;   array → vector
;;;   null → :null    false → nil    true → t
;;;   bytes → (simple-array (unsigned-byte 8) (*))
;;;   tag → cbor-tag (tag 2/3 decoded as bignums)

(defstruct (cbor-tag (:constructor make-cbor-tag (number value)))
  number value)

(defun %u8 (n)
  (make-array n :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(defun %push (buf byte)
  (vector-push-extend byte buf))

(defun %write-uint (buf major n)
  (cond
    ((<= n 23)
     (%push buf (logior (ash major 5) n)))
    ((<= n 255)
     (%push buf (logior (ash major 5) 24))
     (%push buf n))
    ((<= n 65535)
     (%push buf (logior (ash major 5) 25))
     (%push buf (ldb (byte 8 8) n))
     (%push buf (ldb (byte 8 0) n)))
    ((<= n #xffffffff)
     (%push buf (logior (ash major 5) 26))
     (loop for shift from 24 downto 0 by 8
           do (%push buf (ldb (byte 8 shift) n))))
    ((<= n #xffffffffffffffff)
     (%push buf (logior (ash major 5) 27))
     (loop for shift from 56 downto 0 by 8
           do (%push buf (ldb (byte 8 shift) n))))
    (t
     (error 'cbor-encode-error :message "integer does not fit in 64 bits"))))

(defun %write-bytes (buf major octets)
  (%write-uint buf major (length octets))
  (loop for b across octets do (%push buf b)))

#+sbcl
(defun %write-float (buf value)
  (if (typep value 'single-float)
      (let ((bits (sb-kernel:single-float-bits value)))
        (%push buf (logior (ash 7 5) 26))
        (loop for shift from 24 downto 0 by 8
              do (%push buf (ldb (byte 8 shift) (logand bits #xffffffff)))))
      (let ((hi (sb-kernel:double-float-high-bits (float value 1.0d0)))
            (lo (sb-kernel:double-float-low-bits (float value 1.0d0))))
        (%push buf (logior (ash 7 5) 27))
        (loop for shift from 24 downto 0 by 8
              do (%push buf (ldb (byte 8 shift) (logand hi #xffffffff))))
        (loop for shift from 24 downto 0 by 8
              do (%push buf (ldb (byte 8 shift) lo))))))

#-sbcl
(defun %write-float (buf value)
  (declare (ignore buf value))
  (error 'cbor-encode-error :message "CBOR float encode requires SBCL in 0.1.0"))

(defun %alist-p (value)
  (and (consp value)
       (every #'consp value)
       (every (lambda (c) (or (stringp (car c)) (symbolp (car c)))) value)))

(defun %octet-vector-p (value)
  (and (vectorp value)
       (not (stringp value))
       (let ((et (array-element-type value)))
         (or (equal et '(unsigned-byte 8))
             (and (not (eq et t)) (subtypep et '(unsigned-byte 8)))))))

(defun %key-string (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))
    (character (string key))))

(defun encode-value (value buf)
  (cond
    ((eq value :null)
     (%push buf (logior (ash 7 5) 22)))
    ((eq value :undefined)
     (%push buf (logior (ash 7 5) 23)))
    ((null value)
     (%push buf (logior (ash 7 5) 20)))
    ((eq value t)
     (%push buf (logior (ash 7 5) 21)))
    ((cbor-tag-p value)
     (%write-uint buf 6 (cbor-tag-number value))
     (encode-value (cbor-tag-value value) buf))
    ((integerp value)
     (cond
       ((>= value 0) (%write-uint buf 0 value))
       ((>= value (- (expt 2 64)))
        (%write-uint buf 1 (- -1 value)))
       (t (error 'cbor-encode-error :message "integer out of CBOR range"))))
    ((floatp value)
     (%write-float buf value))
    ((stringp value)
     (%write-bytes buf 3 (babel:string-to-octets value :encoding :utf-8)))
    ((%octet-vector-p value)
     (%write-bytes buf 2 value))
    ((hash-table-p value)
     (%write-uint buf 5 (hash-table-count value))
     (maphash (lambda (k v)
                (encode-value (if (or (stringp k) (symbolp k)) (%key-string k) k) buf)
                (encode-value v buf))
              value))
    ((%alist-p value)
     (%write-uint buf 5 (length value))
     (dolist (pair value)
       (encode-value (%key-string (car pair)) buf)
       (encode-value (cdr pair) buf)))
    ((or (vectorp value) (listp value))
     (let ((seq (if (listp value) (coerce value 'vector) value)))
       (%write-uint buf 4 (length seq))
       (loop for item across seq do (encode-value item buf))))
    (t
     (error 'cbor-encode-error
            :message (format nil "cannot encode ~S" (type-of value))))))

(defstruct (%reader (:conc-name %r-) (:constructor %make-reader (octets &optional (index 0))))
  octets index)

(defun %need (reader n)
  (unless (<= (+ (%r-index reader) n) (length (%r-octets reader)))
    (error 'cbor-parse-error :message "truncated CBOR")))

(defun %read-byte (reader)
  (%need reader 1)
  (prog1 (aref (%r-octets reader) (%r-index reader))
    (incf (%r-index reader))))

(defun %read-be (reader n)
  (%need reader n)
  (let ((v 0))
    (dotimes (i n v)
      (setf v (logior (ash v 8) (%read-byte reader))))))

(defun %read-argument (reader info)
  (cond
    ((<= info 23) info)
    ((= info 24) (%read-byte reader))
    ((= info 25) (%read-be reader 2))
    ((= info 26) (%read-be reader 4))
    ((= info 27) (%read-be reader 8))
    ((= info 31) :indefinite)
    (t (error 'cbor-parse-error :message (format nil "reserved additional info ~D" info)))))

(defun %decode-half (bits)
  (let* ((sign (ldb (byte 1 15) bits))
         (exp (ldb (byte 5 10) bits))
         (frac (ldb (byte 10 0) bits))
         (value (cond
                  ((= exp 0)
                   (* (expt 2 -14) (/ frac 1024.0d0)))
                  ((= exp 31)
                   (error 'cbor-parse-error :message "CBOR half NaN/infinity"))
                  (t
                   (* (expt 2 (- exp 15)) (+ 1.0d0 (/ frac 1024.0d0)))))))
    (if (zerop sign) value (- value))))

#+sbcl
(defun %decode-single (bits)
  (sb-kernel:make-single-float (if (> bits #x7fffffff)
                                   (- bits (ash 1 32))
                                   bits)))

#+sbcl
(defun %decode-double (hi lo)
  (sb-kernel:make-double-float (if (> hi #x7fffffff)
                                   (- hi (ash 1 32))
                                   hi)
                               lo))

#-sbcl
(defun %decode-single (bits)
  (declare (ignore bits))
  (error 'cbor-parse-error :message "CBOR float decode requires SBCL in 0.1.0"))

#-sbcl
(defun %decode-double (hi lo)
  (declare (ignore hi lo))
  (error 'cbor-parse-error :message "CBOR float decode requires SBCL in 0.1.0"))

(defun %read-bytes (reader n)
  (%need reader n)
  (let ((out (make-array n :element-type '(unsigned-byte 8))))
    (replace out (%r-octets reader) :start2 (%r-index reader))
    (incf (%r-index reader) n)
    out))

(defun %read-indefinite-bytes (reader major)
  (let ((chunks '()))
    (loop
      (let ((b (%read-byte reader)))
        (when (= b #xff)
          (return))
        (let ((maj (ash b -5))
              (info (logand b #x1f)))
          (unless (= maj major)
            (error 'cbor-parse-error :message "indefinite chunk type mismatch"))
          (let ((n (%read-argument reader info)))
            (when (eq n :indefinite)
              (error 'cbor-parse-error :message "nested indefinite bytes"))
            (push (%read-bytes reader n) chunks)))))
    (let* ((total (reduce #'+ chunks :key #'length))
           (out (make-array total :element-type '(unsigned-byte 8)))
           (i 0))
      (dolist (c (nreverse chunks) out)
        (replace out c :start1 i)
        (incf i (length c))))))

(defun %bytes-to-integer (octets negative)
  (let ((n 0))
    (loop for b across octets do (setf n (logior (ash n 8) b)))
    (if negative (- -1 n) n)))

(defun decode-item (reader)
  (let* ((b (%read-byte reader))
         (major (ash b -5))
         (info (logand b #x1f))
         (arg (%read-argument reader info)))
    (ecase major
      (0 arg)
      (1 (- -1 arg))
      (2
       (if (eq arg :indefinite)
           (%read-indefinite-bytes reader 2)
           (%read-bytes reader arg)))
      (3
       (babel:octets-to-string
        (if (eq arg :indefinite)
            (%read-indefinite-bytes reader 3)
            (%read-bytes reader arg))
        :encoding :utf-8))
      (4
       (if (eq arg :indefinite)
           (let ((acc (make-array 8 :adjustable t :fill-pointer 0)))
             (loop
               (let ((next (aref (%r-octets reader) (%r-index reader))))
                 (when (= next #xff)
                   (%read-byte reader)
                   (return (coerce acc 'vector)))
                 (vector-push-extend (decode-item reader) acc))))
           (let ((out (make-array arg)))
             (dotimes (i arg out)
               (setf (aref out i) (decode-item reader))))))
      (5
       (let ((ht (make-hash-table :test #'equal)))
         (flet ((one-pair ()
                  (let ((k (decode-item reader))
                        (v (decode-item reader)))
                    (setf (gethash k ht) v))))
           (if (eq arg :indefinite)
               (loop
                 (let ((next (aref (%r-octets reader) (%r-index reader))))
                   (when (= next #xff)
                     (%read-byte reader)
                     (return ht))
                   (one-pair)))
               (progn
                 (dotimes (i arg) (one-pair))
                 ht)))))
      (6
       (let ((value (decode-item reader)))
         (cond
           ((and (member arg '(2 3)) (%octet-vector-p value))
            (%bytes-to-integer value (= arg 3)))
           (t (make-cbor-tag arg value)))))
      (7
       (cond
         ((eq arg :indefinite)
          (error 'cbor-parse-error :message "unexpected break"))
         ((= info 20) nil)
         ((= info 21) t)
         ((= info 22) :null)
         ((= info 23) :undefined)
         ((= info 25) (%decode-half arg))
         ((= info 26) (%decode-single arg))
         ((= info 27)
          (%decode-double (ldb (byte 32 32) arg) (ldb (byte 32 0) arg)))
         ((<= info 24)
          (error 'cbor-parse-error :message (format nil "unassigned simple ~D" arg)))
         (t (error 'cbor-parse-error :message (format nil "reserved simple ~D" info))))))))

(defun encode-cbor (value)
  (let ((buf (%u8 64)))
    (encode-value value buf)
    (coerce buf '(simple-array (unsigned-byte 8) (*)))))

(defun decode-cbor (octets)
  (let ((reader (%make-reader octets)))
    (prog1 (decode-item reader)
      (unless (= (%r-index reader) (length octets))
        (error 'cbor-parse-error :message "trailing CBOR bytes")))))
