(defpackage yami.sym
  (:use :cl)
  (:export :gen-sym
           :sym-string
           :string-sym))
(in-package :yami.sym)

(defun revbit (v)
  (dotimes (i 15)
    (rotatef (ldb (byte 1 i) v) (ldb (byte 1 (- 29 i)) v)))
  v)

(defun egcd (a b)
  (if (zerop a)
      (values b 0 1)
      (multiple-value-bind (g y x) (egcd (mod b a) a)
        (values g (- x (* (floor b a) y)) y))))

(defparameter *salt* #x18279131)
(defparameter *inv-salt*
  (multiple-value-bind (a b) (egcd *salt* #x40000000)
    (assert (= a 1))
    (mod b #x40000000)))

(assert (= (logand #x3fffffff (* *salt* *inv-salt*)) 1))

(defun scramble (v)
  (logand #x3fffffff (* (revbit (logand #x3fffffff (* v *salt*))) *inv-salt*)))

(defun prin2 (n)
  (format t "~%~30,'0b" n))

(defvar *id* 0)
(defun gen-sym ()
  (incf *id*))


(defun id-string (id)
  (format nil "~(~4,'0x~)" id))

(defun string-id (string)
  (parse-integer string :radix 16))


(defun sym-check (sym)
  (logand (scramble (logxor sym #x3719)) #xffff))

(defun sym-string (sym)
  (format nil "~(~8,'0x~4,'0x~)"
          (scramble sym)
          (sym-check sym)))

(defun string-sym (string)
  (assert (stringp string))
  (assert (= (length string) 12))
  (let ((sym (scramble (parse-integer string :start 0 :end 8 :radix 16))))
    (assert (= (sym-check sym) (parse-integer string :start 8 :radix 16)))
    sym))


(defun sym-locked-p (sym)
  (= (logand (aref sym 0) #b10000000)
     #b10000000))

(defun sym-verify (sym secret)
  (array= (secret-sym (string-array secret))
          sym))

(defvar *authorized-syms* '())

(defun sym-verified-p (sym)
  (find sym *authorized-syms* :test #'eq))

(defmacro with-sym-verify ((sym secret) &body body)
  `(let* ((sym ,sym)
          (*authorized-syms* (if (sym-verify sym ,secret)
                                 (cons sym *authorized-syms*)
                                 *authorized-syms*)))
       ,@body))
