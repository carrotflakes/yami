(defpackage yami.commands
  (:use :cl)
  (:import-from :yami.parser
                :parse)
  (:import-from :yami.sym
                :string-sym)
  (:export :build))
(in-package :yami.commands)

(defun value (form)
  (ecase (first form)
    (:variable (intern (second form) :keyword))
    (:string (second form))
    (:symbol (string-sym (second form)))))

(defun build (source)
  (loop
    for (command . args) in (parse source)
    collect (cons (intern (string-upcase command) :keyword)
                  (mapcar #'value args))))

(print (build "common x y; var z :018ead2588caa60f206a914558a8f758250650771a4632f2; add x y z; collect z;"))
