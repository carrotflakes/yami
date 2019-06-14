(defpackage yami
  (:use :cl
        :yami.data
        :yami.find)
  (:import-from :yami.commands
                :build)
  (:import-from :yami.sym
                :sym
                :name-sym
                :new-sym
                :sym-auth
                :sym-string
                :sym-secret-string)
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

(defun resolve (form bindings)
  (if (keywordp form)
      (cdr (assoc form bindings))
      form))

(defun stringify (value)
  (etypecase value
    (string (write-to-string value))
    (sym (concatenate 'string ":" (sym-string value)))))

(defun run-commands (request commands &optional bindings)
  (unless commands
    (return-from run-commands))
  (let ((command (pop commands)))
    (case (first command)
      (:common
       (setf bindings
             (nconc (mapcar (lambda (v) (cons v (name-sym (symbol-name v))))
                            (cdr command))
                    bindings))
       (run-commands request commands bindings))
      (:var
       (push (cons (second command) (resolve (third command) bindings))
             bindings)
       (run-commands request commands bindings))
      (:unlock
       (sym-auth (resolve (second command) bindings) (third command))
       (run-commands request commands bindings))
      (:symbol
       (setf bindings
             (nconc (mapcar (lambda (v) (cons v (new-sym)))
                            (cdr command))
                    bindings))
       (run-commands request commands bindings))
      (:locked
       (let ((sym (new-sym t)))
         (push (cons (second command) sym) bindings)
         (push (cons (third command) (sym-secret-string sym)) bindings))
       (run-commands request commands bindings))
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
       (print (loop
                for x in (cdr command)
                collect (stringify (resolve x bindings))))
       (run-commands request commands bindings)))))

(run-commands nil (build "
common a b c;
var x a;
var y 'aaa';
collect x y c;
locked l s;
collect l s;"))
