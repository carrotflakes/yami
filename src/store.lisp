(defpackage yami.store
  (:use :cl)
  (:export :add
           :rm
           :find1))
(in-package :yami.store)

(defvar *edges* '())

(defun load (path)
  )

(defun add (label left right)
  (push (list label left right) *edges*)
  (values))

(defun rm (label left right)
  (setf *edges* (remove (list label left right) *edges* :test #'equal))
  (values))

(defun find1 (label left right)
  (loop
    for (label left right) in *edges*
    when (and (or (
  )