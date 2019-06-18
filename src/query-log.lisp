(defpackage yami.query-log
  (:use :cl)
  (:export :setup
           :push-query-log))
(in-package :yami.query-log)

(defparameter *query-log-stream* nil)

(defun setup (path)
  (setf *query-log-stream*
        (open path
              :direction :output
              :element-type 'base-char
              :external-format :utf-8
              :if-does-not-exist :create
              :if-exists :append)))

(defun push-query-log (string)
  (when *query-log-stream*
    (write string :stream *query-log-stream*)
    (fresh-line *query-log-stream*)
    (force-output *query-log-stream*)))
