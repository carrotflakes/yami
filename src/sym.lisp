(defpackage yami.sym
  (:use :cl)
  (:import-from :md5
                :md5sum-sequence
                :md5sum-string)
  (:export :sym
           :sym=
           :gen-sym
           :gen-locked-sym
           :name-sym
           :sym-check
           :sym-string
           :string-sym
           :sym-locked-p
           :sym-verify
           :sym-verified-p
           :with-sym-verify))
(in-package :yami.sym)

(defparameter +key+ (md5sum-string "yamikey"))

(deftype sym () '(simple-array (unsigned-byte 8) (16)))

(defun array= (v1 v2)
  (and (= (length v1) (length v2))
       (loop
         for i below (length v1)
         unless (= (aref v1 i) (aref v2 i))
         do (return nil)
         finally (return t))))

(defun sym= (v1 v2)
  (array= v1 v2))

(defun array-string (array)
  (format nil "~(~{~2,'0x~}~)"
          (coerce array 'list)))

(defun string-array (string)
  (coerce (loop
            for i below (length string) by 2
            collect (parse-integer string :start i :end (+ i 2) :radix 16))
          '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))))

(defun concat (x y)
  (concatenate '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) x y))


(defun secret-sym (secret)
  (md5sum-sequence (concat secret +key+)))

(defvar *sym-count* 0)

(defun %gen-sym (secret &optional name)
  (let* ((secret-seed (if name
                          (concatenate 'string name "/namedsym")
                          (format nil "secret/~a/~a/~a"
                                  (get-universal-time)
                                  (get-internal-real-time)
                                  (incf *sym-count*))))
         (secret-md5 (md5sum-string (concatenate 'string secret-seed "/yami")))
         (id-md5 (secret-sym secret-md5)))
    (setf (aref id-md5 0) (if secret
                              (logior (aref id-md5 0) #b10000000)
                              (logand (aref id-md5 0) #b01111111)))
    (values id-md5 secret-md5)))

(defun gen-sym ()
  (values (%gen-sym nil)))

(defun gen-locked-sym ()
  (multiple-value-bind (sym secret) (%gen-sym t)
    (values sym (array-string secret))))

(defun name-sym (name)
  (values (%gen-sym nil name)))

(defun sym-check (sym)
  (subseq (md5sum-sequence (concat sym +key+)) 0 8))

(defun sym-string (sym)
  (format nil "~(~{~2,'0x~}~)"
          (nconc (coerce sym 'list)
                 (coerce (sym-check sym) 'list))))

(defun string-sym (string)
  (assert (stringp string))
  (assert (= (length string) 48))
  (let* ((sym (coerce (loop
                        for i below 32 by 2
                        collect (parse-integer string :start i :end (+ i 2) :radix 16))
                      '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (16))))
         (check (coerce (loop
                          for i from 32 below 48 by 2
                          collect (parse-integer string :start i :end (+ i 2) :radix 16))
                        '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (8)))))
    (assert (array= check (sym-check sym)))
    sym))

(defun sym-locked-p (sym)
  (= (logand (aref sym 0) #b10000000)
     #b10000000))

(defun sym-verify (sym secret)
  (array= (secret-sym (string-array secret))
          sym))

(defvar *authorized-syms* '())

(defun sym-verified-p (sym)
  (find sym *authorized-syms* :test #'eq))

(defmacro with-sym-verify ((sym secret) &body body)
  `(let* ((sym ,sym)
          (*authorized-syms* (if (sym-verify sym ,secret)
                                 (cons sym *authorized-syms*)
                                 *authorized-syms*)))
       ,@body))
