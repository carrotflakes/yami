(defpackage yami
  (:use :cl
        :yami.data
        :yami.find)
  (:import-from :yami.commands
                :build)
  (:export :a
           :e
           :with-find
           :find-all
           :root))
(in-package :yami)

(defvar root (a "root"))

(print (build "add 'hoge' 'fuga' 'piyo';"))