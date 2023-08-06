(defpackage yami.commands
  (:use :cl)
  (:import-from :yami.sym
                :string-sym)
  (:export :svar
           :svar-p
           :svar-name
           :svar-value
           :generate-code))
(in-package :yami.commands)

(defstruct svar
  name
  (value nil))

(defvar *bindings*)

(defun value (form)
  (when (numberp form)
    (return-from value form))
  (ecase (first form)
    (:variable (or (cdr (assoc (second form) *bindings* :test #'string=))
                   (let ((svar (make-svar :name (second form))))
                     (push (cons (second form) svar) *bindings*)
                     svar)))
    (:string (second form))
    (:symbol (string-sym (second form)))))

(defun generate-code (ast)
  (loop
    with *bindings* = '()
    for (command . args) in ast
    collect (cons (intern (string-upcase command) :keyword)
                  (mapcar #'value args))))
