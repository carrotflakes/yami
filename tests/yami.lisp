(defpackage yami-test
  (:use :cl
        :yami
        :prove))
(in-package :yami-test)

;; NOTE: To run this test file, execute `(asdf:test-system :yami)' in your Lisp.

(plan nil)

(defvar has (a 'has))
(defvar foods (a 'foods))
(defvar types (a 'types))
(defvar users (a 'users))

(e has root foods)
(e has foods (a 'strawberry))
(e has root types)
(e has types (a 'list))
(e has types (a 'set))
(e has root users)
(e has users (a 'carrotflakes))

(with-find
    (v1)
  (e has root v1)
  (find-all
   (lambda () (print v1))))

(finalize)
