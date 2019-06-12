(defpackage yami.data
  (:use :cl)
  (:export :edge
           :edge-p
           :edge-id
           :edge-label
           :edge-left
           :edge-right
           :*atoms*
           :*edges*
           :*edge-id*
           :a
           :e
           :rm))
(in-package :yami.data)

(defstruct edge
  id
  label
  left
  right)

(defvar *atoms* '())
(defvar *edges* '())
(defvar *edge-id* 0)

(defun a (value)
  (let ((atom (etypecase value
                (string value)
                (number value)
                (symbol (gensym (symbol-name value))))))
    (pushnew atom *atoms*)
    atom))

(defun e (label left right)
  (dolist (edge *edges*)
    (when (and (equal (edge-label edge) label)
               (equal (edge-left edge) left)
               (equal (edge-right edge) right))
      (return-from e edge)))
  (let ((edge (make-edge :id (incf *edge-id*)
                         :label label
                         :left left
                         :right right)))
    (push edge *edges*)
    edge))

(defun rm (edge)
  (setf *edges* (remove edge *edges*))
  nil)
