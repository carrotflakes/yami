(defpackage yami.sym
  (:use :cl
        :yami.data
        :yami.find)
  (:import-from :yami.commands
                :build)
  (:export :a
           :e
           :with-find
           :find-all
           :root))
(in-package :yami.sym)

(defstruct sym
  id
  check
  secret
  (authorized nil))

(defun concat (x y)
  (concatenate '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) x y))

(defconstant +key+ (md5:md5sum-string "yamikey"))

(defun make-check (id)
  (subseq (md5:md5sum-sequence (concat id +key+)) 0 8))

(defun secret-id (secret)
  (md5:md5sum-sequence (concat secret +key+)))

(defun new-sym (&optional secret)
  (let* ((secret-string (format nil "yami/~a/~a"
                                (get-universal-time)
                                (get-internal-real-time)))
         (secret-md5 (md5:md5sum-string secret-string))
         (id-md5 (secret-id secret-md5)))
    (setf (aref id-md5 0) (if secret
                              (logior (aref id-md5 0) #b10000000)
                              (logand (aref id-md5 0) #b01111111)))
    (make-sym :id id-md5
              :check (make-check id-md5)
              :secret (and secret secret-md5))))

(defun sym-string (sym)
  (format nil "~(~{~2,'0x~}~)"
          (nconc (coerce (sym-id sym) 'list)
                 (coerce (sym-check sym) 'list))))

(defun vector= (v1 v2)
  (and (= (length v1) (length v2))
       (loop
         for i below (length v1)
         unless (= (aref v1 i) (aref v2 i))
         do (return nil)
         finally (return t))))

(defun string-sym (string)
  (assert (stringp string))
  (assert (= (length string) 48))
  (let ((id (coerce (loop
                      for i below 32 by 2
                      collect (parse-integer string :start i :end (+ i 2) :radix 16))
                    '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (16))))
        (check (coerce (loop
                         for i from 32 below 48 by 2
                         collect (parse-integer string :start i :end (+ i 2) :radix 16))
                       '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (8)))))
    (assert (vector= check (make-check id)))
    (make-sym :id id
              :check check
              :secret (not (zerop (logand (aref id 0) #b10000000))))))

(defun sym-auth (sym secret)
  (setf (sym-authorized sym) (vector= (secret-id secret) (sym-id sym))))

(let* ((sym (new-sym nil))
       (str (sym-string sym))
       (sym* (string-sym str)))
  (print sym)
  (print str)
  (print sym*))

(let* ((sym (new-sym t))
       (str (sym-string sym))
       (sym* (string-sym str)))
  (print sym)
  (print str)
  (print sym*)
  (print (sym-auth sym* #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
  (print (sym-auth sym* (sym-secret sym))))