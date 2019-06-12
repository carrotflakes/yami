(defpackage yami.find
  (:use :cl
        :yami.data)
  (:export :with-find
           :find-all))
(in-package :yami.find)

(defvar *global-atoms*)
(defvar *global-edges*)

(defstruct svar
  (value nil))

(defun unwrap (atom)
  (if (svar-p atom)
      (or (svar-value atom) atom)
      atom))

(defun %find-all (edges cb)
  (unless edges
    (funcall cb)
    (return-from %find-all))
  (let* ((edge (pop edges))
         (label (unwrap (edge-label edge)))
         (left (unwrap (edge-left edge)))
         (right (unwrap (edge-right edge))))
    (dolist (edge *global-edges*)
      (when (and (or (svar-p label) (equal (edge-label edge) label))
                 (or (svar-p left) (equal (edge-left edge) left))
                 (or (svar-p right) (equal (edge-right edge) right)))
        (when (svar-p label) (setf (svar-value label) (edge-label edge)))
        (when (svar-p left) (setf (svar-value left) (edge-left edge)))
        (when (svar-p right) (setf (svar-value right) (edge-right edge)))
        (%find-all edges cb)
        (when (svar-p label) (setf (svar-value label) nil))
        (when (svar-p left) (setf (svar-value left) nil))
        (when (svar-p right) (setf (svar-value right) nil))))))

(defun find-all (cb)
  (%find-all *edges* cb)
  nil)

(defmacro with-find (variable-list &body body)
  `(let ((*global-atoms* *atoms*)
         (*global-edges* *edges*)
         (*atoms* '())
         (*edges* '())
         (*edge-id* 0))
     (let ,(loop
             for v in variable-list
             collect (list v '(make-svar)))
       ,@body)))
