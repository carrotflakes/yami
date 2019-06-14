(defpackage yami.store
  (:use :cl)
  (:import-from :yami.sym
                :array=
                :sym
                :sym-id
                :id-sym
                :sym-authorized
                :sym-string
                :sym-secret-string)
  (:import-from :yami.commands
                :svar
                :svar-p
                :svar-value)
  (:export :add
           :rm
           :finde))
(in-package :yami.store)

(defvar *edges* '())

;(defun load (path)
;  )

(defun e (x y)
  (if (and (typep x '(simple-array (unsigned-byte 8) (*)))
           (typep y '(simple-array (unsigned-byte 8) (*))))
      (array= x y)
      (equal x y)))

(defun ee (x y)
  (or (eq x y)
      (and (e (first x) (first y))
           (e (second x) (second y))
           (e (third x) (third y)))))

(defun ensure-sym (x)
  (if (typep x '(simple-array (unsigned-byte 8) (*)))
      (id-sym x)
      x))

(defun clean (x)
  (etypecase x
    (string x)
    (sym (sym-id x))
    (svar x)))

(defun add (label left right)
  ; TODO: ensure authorized, duplication check
  (push (list (clean label) (clean left) (clean right)) *edges*)
  (values))

(defun rm (label left right)
  (setf label (clean label)
        left (clean left)
        right (clean right))
  ; TODO: holed edge
  (setf *edges* (remove (list label left right) *edges* :test #'ee))
  (values))

(defun finde (n label left right)
  (setf label (clean label)
        left (clean left)
        right (clean right))
  (loop
    while (plusp n)
    for edge in *edges*
    when (and (or (svar-p label) (e label (first edge)))
              (or (svar-p left) (e left (second edge)))
              (or (svar-p right) (e right (third edge))))
    collect (progn
              (decf n)
              (mapcar #'ensure-sym edge))))
