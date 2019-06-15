(defpackage yami.store
  (:use :cl)
  (:import-from :yami.sym
                :sym=
                :sym
                :sym-string
                :string-sym)
  (:import-from :yami.commands
                :svar
                :svar-p
                :svar-value)
  (:export :setup
           :add
           :rm
           :finde))
(in-package :yami.store)

(defvar *edges* '())

(defvar *change-log-stream* nil)

(defun setup (path)
  (when *change-log-stream*
    (close *change-log-stream*)
    (setf *change-log-stream* nil))
  (with-open-file (stream path
                          :direction :input
                          :element-type 'base-char
                          :external-format :utf-8
                          :if-does-not-exist :create)
    (setf *edges* '())
    (loop
      for form = (read stream nil)
      while form
      for args = (mapcar (lambda (x) (if (typep x 'keyword) (string-sym (symbol-name x)) x))
                         (cdr form))
      do (cond ((string= (car form) "+")
                (apply #'add args))
               ((string= (car form) "-")
                (setf *edges* (delete args *edges* :test #'ee))))))
  (setf *change-log-stream* (open path
                                  :direction :output
                                  :element-type 'base-char
                                  :external-format :utf-8
                                  :if-exists :append)))

(defun push-change-log (form)
  (when *change-log-stream*
    (write (mapcar (lambda (x) (if (typep x 'sym) (intern (sym-string x) :keyword) x)) form)
           :stream *change-log-stream*)
    (fresh-line *change-log-stream*)
    (force-output *change-log-stream*)))


(defun e (x y)
  (if (and (typep x 'sym)
           (typep y 'sym))
      (sym= x y)
      (equal x y)))

(defun ee (x y)
  (or (eq x y)
      (and (e (first x) (first y))
           (e (second x) (second y))
           (e (third x) (third y)))))

(defun add (label left right)
  ; TODO: ensure authorized, duplication check
  (push (list label left right) *edges*)
  (push-change-log (list '+ label left right))
  (values))

(defun rm (n label left right)
  (let ((result (finde n label left right)))
    (setf *edges* (set-difference *edges* result :test #'ee))
    (dolist (edge result)
      (push-change-log (list* '- edge)))
    result))

(defun finde (n label left right)
  (loop
    while (plusp n)
    for edge in *edges*
    when (and (or (svar-p label) (e label (first edge)))
              (or (svar-p left) (e left (second edge)))
              (or (svar-p right) (e right (third edge))))
    collect (progn
              (decf n)
              edge)))
