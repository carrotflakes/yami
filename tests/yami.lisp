(defpackage yami-test
  (:use :cl
        :yami
        :prove))
(in-package :yami-test)

;; NOTE: To run this test file, execute `(asdf:test-system :yami)' in your Lisp.

(plan nil)


(print "+++++++++++++++++++")(terpri)
(run-commands nil (query-code "
common a b c;
var x a;
var y 'aaa';
collect x y c;
locked l s;
add a b c;
add 'a' 'b' 'c';
rm a b c;
collect l s;
"))
(print "+++++++++++++++++++")(terpri)

(run-commands nil (query-code "
findAll x y z;
collect x y z;
"))

(print "+++++++++++++++++++")(terpri)
(run-commands nil (query-code "
add 'has' 'root' 'users';
add 'has' 'users' 'carrotflakes';
add 'has' 'root' 'food';
add 'has' 'food' 'ramen';
add 'has' 'food' 'udon';
add 'has' 'food' 'soba';
add 'has' 'food' 'takuan';

findAll 'has' 'food' x;
collect x;

add 'like' 'carrotflakes' x;
"))
(print "+++++++++++++++++++")(terpri)

(run-commands nil (query-code "
findAll x y z;
collect x y z;
"))

(finalize)
