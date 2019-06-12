(defpackage yami
  (:use :cl
        :yami.data
        :yami.find)
  (:export :a
           :e
           :with-find
           :find-all
           :root))
(in-package :yami)

(defvar root (a "root"))
