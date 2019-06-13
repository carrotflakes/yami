(defpackage yami.commands
  (:use :cl)
  (:import-from :yami.parser
                :parse)
  (:export :build))
(in-package :yami.commands)

(defun build (source)
  (loop
    for (command . args) in (parse source)
    collect (cons (intern (string-upcase command) :keyword) args)))
