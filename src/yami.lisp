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

(defstruct request
  paid-calorie
  remain-calorie
  source
  (output nil))

(defstruct state
  request
  (bindings nil)
  commands)


(defvar source "add 'hoge' 'fuga' 'piyo';")
(defvar req (make-request :paid-calorie 100
                          :remain-calorie 100
                          :source source))
(defvar state (make-state :request req
                          :commands (build (request-source req))))


(defun run-commands (request commands &optional bindings)
  (unless commands
    (return-from run-commands))
  (let ((command (pop commands)))
    (case (first command)
      (:common
       ;(run-commands request commands (nconc () bindings))
      (:var
       )
      (:unlock
       )
      (:symbol
       )
      (:locked
       )
      (:add
       )
      (:rm
       )
      (:find1
       )
      (:findSome
       )
      (:findAll
       )
      (:collect
       ))))
