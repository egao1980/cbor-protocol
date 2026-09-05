(in-package #:cbor-protocol/tests)

(defun %hex (string)
  (let ((clean (remove-if (lambda (c) (find c " \t\n")) string))
        (out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for i from 0 below (length clean) by 2
          do (vector-push-extend (parse-integer clean :start i :end (+ i 2) :radix 16) out))
    (coerce out '(vector (unsigned-byte 8)))))

(defun %lisp= (a b)
  (cond
    ((and (hash-table-p a) (hash-table-p b))
     (and (= (hash-table-count a) (hash-table-count b))
          (loop for k being the hash-keys of a using (hash-value v)
                always (and (nth-value 1 (gethash k b))
                            (%lisp= v (gethash k b))))))
    ((and (vectorp a) (not (stringp a))
          (vectorp b) (not (stringp b)))
     (and (= (length a) (length b))
          (loop for i from 0 below (length a)
                always (%lisp= (aref a i) (aref b i)))))
    ((and (floatp a) (floatp b))
     (< (abs (- a b)) 1d-6))
    ((and (numberp a) (numberp b)) (= a b))
    (t (equal a b))))

(deftest rfc8949-appendix-a
  (dolist (row '(("00" 0)
                 ("01" 1)
                 ("0a" 10)
                 ("17" 23)
                 ("1818" 24)
                 ("1819" 25)
                 ("1864" 100)
                 ("1903e8" 1000)
                 ("1a000f4240" 1000000)
                 ("20" -1)
                 ("29" -10)
                 ("3863" -100)
                 ("f4" nil)
                 ("f5" t)
                 ("f6" :null)
                 ("60" "")
                 ("6161" "a")
                 ("6449455446" "IETF")
                 ("80" #())
                 ("83010203" #(1 2 3))))
    (destructuring-bind (hex value) row
      (ok (%lisp= value (decode (%hex hex))) hex)
      (ok (equalp (%hex hex) (encode value)) hex)))
  (ok (%lisp= (make-hash-table :test #'equal) (decode (%hex "a0"))))
  (ok (equalp (%hex "a0") (encode (make-hash-table :test #'equal)))))

(deftest map-roundtrip
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "a" ht) 1
          (gethash "b" ht) #(2 3)
          (gethash "z" ht) :null)
    (ok (%lisp= ht (decode (encode ht))))))

(deftest bytes-and-tag
  (let ((raw (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (ok (equalp raw (decode (encode raw)))))
  (let ((tag (make-cbor-tag 32 "http://example.com")))
    (let ((back (decode (encode tag))))
      (ok (cbor-tag-p back))
      (ok (= 32 (cbor-tag-number back)))
      (ok (string= "http://example.com" (cbor-tag-value back))))))

(deftest predicates
  (ok (null-p :null))
  (ok (true-p t))
  (ok (false-p nil))
  (ng (false-p :null)))
