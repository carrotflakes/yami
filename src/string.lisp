(defpackage yami.string
  (:use :cl)
  (:import-from :flexi-streams
                :string-to-octets)
  (:export :gen-string-id
           :string-id-p))
(in-package :yami.string)

(defun string-id (string)
  (let ((array (string-to-octets string :external-format :utf-8))
        (id 0))
    (dotimes (i (length array))
      (setf id (logand (+ (* id #x0101)
                          (* (aref array i) (+ 1234 i)))
                       #x3fffffff)))
    (+ id #x40000000)))

(defun string-id-p (id)
  (<= #x40000000 id))


(defun test (&optional (n 1000000))
  (length
   (remove-duplicates
    (loop
      for i below n
      collect (string-id (write-to-string i))))))
