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
                :gen-sym
                :gen-locked-sym
                :name-sym
                :with-sym-verify
                :sym-verified-p
                :sym-string)
  (:import-from :yami.store
                :setup
                :add
                :rm
                :finde)
  (:export :query-code
           :run-commands
           :setup))
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

(defmacro aux (form)
  `(loop
     for edge in ,form
     do (when (svar-p label) (setf (svar-value label) (first edge)))
        (when (svar-p left) (setf (svar-value left) (second edge)))
        (when (svar-p right) (setf (svar-value right) (third edge)))
        (run-commands request commands)
        (when (svar-p label) (setf (svar-value label) nil))
        (when (svar-p left) (setf (svar-value left) nil))
        (when (svar-p right) (setf (svar-value right) nil))))

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
       (with-sym-verify ((resolve (second command)) (resolve (third command)))
         (when (sym-verified-p (resolve (second command)))
           (run-commands request commands))))
      (:symbol
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) (gen-sym)))
       (run-commands request commands)
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) nil)))
      (:locked
       (multiple-value-bind (sym secret) (gen-locked-sym)
         (setf (svar-value (second command)) sym
               (svar-value (third command)) secret))
       (run-commands request commands)
       (setf (svar-value (second command)) nil
             (svar-value (third command)) nil))
      (:add
       (add (resolve (second command))
            (resolve (third command))
            (resolve (fourth command))) ; TODO ensure no variable
       (run-commands request commands))
      (:rm
       (let ((label (second command))
             (left (third command))
             (right (fourth command)))
         (aux (rm 1
                  (resolve (second command))
                  (resolve (third command))
                  (resolve (fourth command))))))
      (:rmAll
       (let ((label (second command))
             (left (third command))
             (right (fourth command)))
         (aux (rm 10000000
                  (resolve (second command))
                  (resolve (third command))
                  (resolve (fourth command))))))
      (:find1
       (let ((label (resolve (second command)))
             (left (resolve (third command)))
             (right (resolve (fourth command))))
         (aux (finde 1 label left right))))
      (:findSome
       )
      (:findAll
       (let ((label (resolve (second command)))
             (left (resolve (third command)))
             (right (resolve (fourth command))))
         (aux (finde 10000000 label left right))))
      (:collect
       (format t "~{~a~^ ~};~%"
               (loop
                 for x in (cdr command)
                 collect (stringify (resolve x)))) ; svar渡ってくる問題
       (run-commands request commands)))))
