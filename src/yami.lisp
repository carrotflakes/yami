(defpackage yami
  (:use :cl))
(in-package :yami)

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

(defvar root (a "root"))

(defun rm (edge)
  (setf *edges* (remove edge *edges*))
  nil)

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

(defvar has (a 'has))
(defvar foods (a 'foods))
(defvar types (a 'types))
(defvar users (a 'users))

(e has root foods)
(e has foods (a 'strawberry))
(e has root types)
(e has types (a 'list))
(e has types (a 'set))
(e has root users)
(e has users (a 'carrotflakes))

(with-find
    (v1)
  (e has root v1)
  (find-all
   (lambda () (print v1))))
