(defpackage yami
  (:use :cl)
  (:import-from :yami.parser
                :parse)
  (:import-from :yami.commands
                :svar-p
                :svar-name
                :svar-value
                :generate-code)
  (:import-from :yami.sym
                :sym
                :name-sym
                :new-sym
                :sym-auth
                :sym-string
                :sym-secret-string)
  (:import-from :yami.store
                :add
                :rm
                :finde)
  (:export :query-code
           :run-commands))
(in-package :yami)

#|
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
|#

(defun query-code (source)
  (generate-code (parse source)))

(defun resolve (form)
  (if (svar-p form)
      (or (svar-value form) form)
      form))

(defun stringify (value)
  (etypecase value
    (string (write-to-string value))
    (sym (concatenate 'string ":" (sym-string value)))))

(defun run-commands (request commands)
  (unless commands
    (return-from run-commands))
  (let ((command (pop commands)))
    (case (first command)
      (:common
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) (name-sym (svar-name svar))))
       (run-commands request commands)
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) nil)))
      (:var
       (setf (svar-value (second command)) (resolve (third command)))
       (run-commands request commands)
       (setf (svar-value (second command)) nil))
      (:unlock
       (sym-auth (resolve (second command)) (resolve (third command)))
       (run-commands request commands))
      (:symbol
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) (new-sym)))
       (run-commands request commands)
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) nil)))
      (:locked
       (let ((sym (new-sym t)))
         (setf (svar-value (second command)) sym
               (svar-value (third command)) (sym-secret-string sym)))
       (run-commands request commands)
       (setf (svar-value (second command)) nil
             (svar-value (third command)) nil))
      (:add
       (add (resolve (second command))
            (resolve (third command))
            (resolve (fourth command))) ; TODO ensure no variable
       (run-commands request commands))
      (:rm ; rm 1, 5, all
       (rm (resolve (second command))
           (resolve (third command))
           (resolve (fourth command)))
       (run-commands request commands))
      (:find1
       (let ((label (resolve (second command)))
             (left (resolve (third command)))
             (right (resolve (fourth command))))
         (loop
           for edge in (finde 1 label left right)
           do (when (svar-p label) (setf (svar-value label) (first edge)))
              (when (svar-p left) (setf (svar-value left) (second edge)))
              (when (svar-p right) (setf (svar-value right) (third edge)))
              (run-commands request commands)
              (when (svar-p label) (setf (svar-value label) nil))
              (when (svar-p left) (setf (svar-value left) nil))
              (when (svar-p right) (setf (svar-value right) nil)))))
      (:findSome
       )
      (:findAll
       (let ((label (resolve (second command)))
             (left (resolve (third command)))
             (right (resolve (fourth command))))
         (loop
           for edge in (finde 10000000 label left right)
           do (when (svar-p label) (setf (svar-value label) (first edge)))
              (when (svar-p left) (setf (svar-value left) (second edge)))
              (when (svar-p right) (setf (svar-value right) (third edge)))
              (run-commands request commands)
              (when (svar-p label) (setf (svar-value label) nil))
              (when (svar-p left) (setf (svar-value left) nil))
              (when (svar-p right) (setf (svar-value right) nil)))))
      (:collect
       (format t "~{~a~^ ~};~%"
               (loop
                 for x in (cdr command)
                 collect (stringify (resolve x))))
       (run-commands request commands)))))
