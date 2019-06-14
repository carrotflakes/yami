(defpackage yami.store
  (:use :cl)
  (:import-from :yami.sym
                :array=
                :sym
                :sym-id
                :sym-authorized
                :sym-string
                :sym-secret-string)
  (:import-from :yami.commands
                :svar
                :svar-p
                :svar-value)
  (:export :add
           :rm
           :find1))
(in-package :yami.store)

(defvar *edges* '())

;(defun load (path)
;  )

(defun e (x y)
  (if (and (arrayp x) (arrayp y))
      (array= x y)
      (equal x y)))

(defun ee (x y)
  (or (eq x y)
      (and (e (first x) (first y))
           (e (second x) (second y))
           (e (third x) (third y)))))

(defun clean (x)
  (etypecase x
    (string x)
    (sym (sym-id x))
    (keyword x)))

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

(defun find1 (n label left right)
  (setf label (clean label)
        left (clean left)
        right (clean right))
  '(loop
    for edge in *edges*
    when nil
    do label))
