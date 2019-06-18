(defpackage yami.store
  (:use :cl)
  (:import-from :yami.string
                :string-id
                :string-id-p)
  (:import-from :yami.commands
                :svar
                :svar-p
                :svar-value)
  (:export :gen-string-id
           :id-string
           :setup
           :add
           :rm
           :finde))
(in-package :yami.store)

(defparameter *table* (make-hash-table :test 'eq))

(defun gen-string-id (string)
  (let* ((id (string-id string))
         (stored-string (gethash id *table*)))
    ; conflict check
    (loop
      while (and stored-string (string/= stored-string string))
      do (print "string rehash" *error-output*)
         (setf id (+ (logand (1+ id) #x3fffffff)
                     #x40000000)
               stored-string (gethash id *table*)))
    (unless stored-string
      (push-change-log string))
    (setf (gethash id *table*) string)
    id))

(defun id-string (id)
  (gethash id *table*))


(defparameter *edges-size* (ash 1 15))
(defparameter *edges* (make-array (* *edges-size* 3)
                                  :element-type '(unsigned-byte 32)
                                  :initial-element 0))
(defparameter *edges-position* -1)

(defparameter *change-log-stream* nil)

(defun restore-id (id)
  (unless (string-id-p id)
    (setf yami.sym::*id* (max id yami.sym::*id*))))

(defun setup (path)
  (when *change-log-stream*
    (close *change-log-stream*)
    (setf *change-log-stream* nil))
  (with-open-file (stream path
                          :direction :input
                          :element-type 'base-char
                          :external-format :utf-8
                          :if-does-not-exist :create)
    (fill *edges* 0)
    (loop
      for form = (read stream nil)
      while form
      do (cond ((stringp form)
                (setf (gethash (string-id form) *table*) form))
               ((string= (car form) "+")
                (mapc #'restore-id (cdr form))
                (apply #'add (cdr form)))
               ((string= (car form) "-")
                (apply #'rm 1 (cdr form))))))
  (setf *change-log-stream* (open path
                                  :direction :output
                                  :element-type 'base-char
                                  :external-format :utf-8
                                  :if-exists :append)))

(defun push-change-log (form)
  (when *change-log-stream*
    (write form :stream *change-log-stream*)
    (fresh-line *change-log-stream*)
    (force-output *change-log-stream*)))


(defun pack (i label left right)
  (make-array 4
              :element-type '(unsigned-byte 32)
              :initial-contents (list i label left right)))

(defun add (label left right)
  ; TODO: ensure authorized, duplication check
  (setf (aref *edges* (incf *edges-position*)) label
        (aref *edges* (incf *edges-position*)) left
        (aref *edges* (incf *edges-position*)) right)
  (push-change-log (list '+ label left right))
  (values))

(defun finde (n label left right)
  (loop
    while (plusp n)
    for i below (* *edges-size* 3) by 3
    when (and (or (svar-p label) (eq label (aref *edges* i)))
              (or (svar-p left) (eq left (aref *edges* (+ i 1))))
              (or (svar-p right) (eq right (aref *edges* (+ i 2))))
              (or (/= (aref *edges* i) 0)
                  (/= (aref *edges* (+ i 1)) 0)
                  (/= (aref *edges* (+ i 2)) 0)))
    collect (progn
              (decf n)
              (pack i (aref *edges* i) (aref *edges* (+ i 1)) (aref *edges* (+ i 2))))))

(defun rm (n label left right)
  (let ((result (finde n label left right)))
    (loop
      for pack in result
      for i = (aref pack 0)
      do (fill *edges* 0 :start i :end (+ i 3)))
    (dolist (edge result)
      (push-change-log (list* '- (cdr (coerce edge 'list)))))
    result))
