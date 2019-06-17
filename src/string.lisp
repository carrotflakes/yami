(defpackage yami.string
  (:use :cl)
  (:import-from :flexi-streams
                :string-to-octets)
  (:export :gen-string-id
           :string-id-p))
(in-package :yami.string)

(defun string-id (string)
  (let ((array (string-to-octets string))
        (id 1000000001))
    (dotimes (i (length array))
      (setf id (logxor id (* (+ (aref array i) (* i) 86432) 123454321))))
    (+ (logand id #x3fffffff)
       #x40000000)))

(defun string-id-p (id)
  (<= #x40000000 id))
