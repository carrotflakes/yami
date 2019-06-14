(defpackage yami.sym
  (:use :cl)
  (:import-from :md5
                :md5sum-sequence
                :md5sum-string)
  (:export :array=
           :sym
           :sym-p
           :sym=
           :sym-id
           :sym-secret
           :sym-authorized
           :name-sym
           :new-sym
           :sym-string
           :string-sym
           :sym-auth
           :sym-secret-string))
(in-package :yami.sym)

(defparameter +key+ (md5sum-string "yamikey"))

(defun array= (v1 v2)
  (and (= (length v1) (length v2))
       (loop
         for i below (length v1)
         unless (= (aref v1 i) (aref v2 i))
         do (return nil)
         finally (return t))))

(defun array-string (array)
  (format nil "~(~{~2,'0x~}~)"
          (coerce array 'list)))

(defun string-array (string)
  (coerce (loop
            for i below (length string) by 2
            collect (parse-integer string :start i :end (+ i 2) :radix 16))
          '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))))

(defstruct sym
  id
  check
  secret
  authorized)

(defun sym= (s1 s2)
  (array= (sym-id s1) (sym-id s2)))

(defun concat (x y)
  (concatenate '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) x y))

(defun make-check (id)
  (subseq (md5sum-sequence (concat id +key+)) 0 8))

(defun secret-id (secret)
  (md5sum-sequence (concat secret +key+)))

(defun name-sym (name &optional secret)
  (let* ((secret-md5 (md5sum-string (concatenate 'string name "/yami")))
         (id-md5 (secret-id secret-md5)))
    (setf (aref id-md5 0) (if secret
                              (logior (aref id-md5 0) #b10000000)
                              (logand (aref id-md5 0) #b01111111)))
    (make-sym :id id-md5
              :check (make-check id-md5)
              :secret (and secret secret-md5)
              :authorized t)))

(defun sym-secret-string (sym)
  (array-string (sym-secret sym)))

(defun new-sym (&optional secret)
  (name-sym (format nil "yami/~a/~a"
                    (get-universal-time)
                    (get-internal-real-time))
            secret))

(defun sym-string (sym)
  (format nil "~(~{~2,'0x~}~)"
          (nconc (coerce (sym-id sym) 'list)
                 (coerce (sym-check sym) 'list))))

(defun string-sym (string)
  (assert (stringp string))
  (assert (= (length string) 48))
  (let* ((id (coerce (loop
                       for i below 32 by 2
                       collect (parse-integer string :start i :end (+ i 2) :radix 16))
                     '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (16))))
         (check (coerce (loop
                          for i from 32 below 48 by 2
                          collect (parse-integer string :start i :end (+ i 2) :radix 16))
                        '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (8))))
         (secret (not (zerop (logand (aref id 0) #b10000000)))))
    (assert (array= check (make-check id)))
    (make-sym :id id
              :check check
              :secret secret
              :authorized (not secret))))

(defun sym-auth (sym secret)
  (setf (sym-authorized sym)
        (array= (secret-id (string-array secret))
                (sym-id sym))))
