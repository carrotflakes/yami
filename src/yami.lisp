(defpackage yami
  (:use :cl)
  (:import-from :yami.parser
                :parse)
  (:import-from :yami.commands
                :svar
                :svar-p
                :svar-name
                :svar-value
                :generate-code)
  (:import-from :yami.sym
                :gen-sym
                :sym-string)
  (:import-from :yami.string
                :string-id-p)
  (:import-from :yami.store
                :gen-string-id
                :id-string
                :add
                :rm
                :finde)
  (:import-from :yami.query-log
                :push-query-log)
  (:export :query-code
           :run-commands
           :setup))
(in-package :yami)


(defun setup (change-log-path query-log-path)
  (yami.store:setup change-log-path)
  (when query-log-path
    (yami.query-log:setup query-log-path)))

(defun query-code (source)
  (generate-code (parse source)))

(defun resolve (form)
  (etypecase form
    (number form)
    (string (gen-string-id form))
    (svar (or (svar-value form) form))))

(defun stringify (value)
  (etypecase value
    (number (if (string-id-p value)
                (write-to-string (id-string value))
                (concatenate 'string ":" (sym-string value))))
    (string (write-to-string value))
    (svar (if (svar-value value)
              (stringify (svar-value value))
              (concatenate 'string "?" (svar-name value))))))

(defmacro aux (form)
  `(dolist (edge ,form)
     (when (svar-p label) (setf (svar-value label) (aref edge 1)))
     (when (svar-p left) (setf (svar-value left) (aref edge 2)))
     (when (svar-p right) (setf (svar-value right) (aref edge 3)))
     (run-commands commands)
     (when (svar-p label) (setf (svar-value label) nil))
     (when (svar-p left) (setf (svar-value left) nil))
     (when (svar-p right) (setf (svar-value right) nil))))

(defun run-commands (commands)
  (unless commands
    (return-from run-commands))
  (let ((command (pop commands)))
    (case (first command)
      ;; (:common
      ;;  (loop
      ;;    for svar in (cdr command)
      ;;    do (setf (svar-value svar) (name-sym (svar-name svar))))
      ;;  (run-commands commands)
      ;;  (loop
      ;;    for svar in (cdr command)
      ;;    do (setf (svar-value svar) nil)))
      (:var
       (setf (svar-value (second command)) (resolve (third command)))
       (run-commands commands)
       (setf (svar-value (second command)) nil))
      ;; (:unlock
      ;;  (with-sym-verify ((resolve (second command)) (resolve (third command)))
      ;;    (when (sym-verified-p (resolve (second command)))
      ;;      (run-commands commands))))
      (:symbol
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) (gen-sym)))
       (run-commands commands)
       (loop
         for svar in (cdr command)
         do (setf (svar-value svar) nil)))
      ;; (:locked
      ;;  (multiple-value-bind (sym secret) (gen-locked-sym)
      ;;    (setf (svar-value (second command)) sym
      ;;          (svar-value (third command)) secret))
      ;;  (run-commands commands)
      ;;  (setf (svar-value (second command)) nil
      ;;        (svar-value (third command)) nil))
      (:add
       (flet ((resolve* (x)
                (setf x (resolve x))
                (when (svar-p x) (error "Variable cannot be added"))
                x))
         (add (resolve* (second command))
              (resolve* (third command))
              (resolve* (fourth command)))) ; TODO ensure no variable
       (run-commands commands))
      (:rm
       (let ((label (third command))
             (left (fourth command))
             (right (fifth command)))
         (aux (rm (second command)
                  (resolve (third command))
                  (resolve (fourth command))
                  (resolve (fifth command))))))
      (:find
       (let ((label (resolve (third command)))
             (left (resolve (fourth command)))
             (right (resolve (fifth command))))
         (aux (finde (second command) label left right))))
      (:collect
       (format t "~{~a~^ ~};~%"
               (loop
                 for x in (cdr command)
                 collect (stringify x))) ; svar渡ってくる問題
       (run-commands commands)))))

(defun run-query (query)
  (push-query-log query)
  (run-commands (query-code query)))
