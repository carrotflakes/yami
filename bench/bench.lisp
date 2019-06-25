(ql:quickload :yami)

(use-package :yami)

(print "Add 1000 edges:")
(time
 (dotimes (i 1000)
   (run-query (format nil "
symbol s;
add 'foobar' '~a' s;" i))))
(terpri)

(print "FindAll 100 times:")
(time
 (with-output-to-string (*standard-output*)
   (dotimes (i 100)
     (run-query (format nil "findAll 'foobar' '~a' x; collect x;" (* i 100))))))
(terpri)
