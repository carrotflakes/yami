(defpackage yami.commands
  (:use :cl)
  (:import-from :yami.parser
                :parse)
  (:import-from :yami.sym
                :string-sym)
  (:export :build
           :svar
           :svar-p
           :svar-name
           :svar-value))
(in-package :yami.commands)

(defstruct svar
  name
  (value nil))

(defvar *bindings*)

(defun value (form)
  (ecase (first form)
    (:variable (or (cdr (assoc (second form) *bindings* :test #'string=))
                   (let ((svar (make-svar :name (second form))))
                     (push (cons (second form) svar) *bindings*)
                     svar)))
    (:string (second form))
    (:symbol (string-sym (second form)))))

(defun build (source)
  (loop
    with *bindings* = '()
    for (command . args) in (parse source)
    collect (cons (intern (string-upcase command) :keyword)
                  (mapcar #'value args))))

(print (build "common x y; var z :018ead2588caa60f206a914558a8f758250650771a4632f2; add x y z; collect z;"))
