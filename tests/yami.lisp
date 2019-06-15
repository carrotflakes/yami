(defpackage yami-test
  (:use :cl
        :yami
        :prove))
(in-package :yami-test)

;; NOTE: To run this test file, execute `(asdf:test-system :yami)' in your Lisp.

(plan nil)


(defun f (source)
  (princ "query +++++++++++++++++++")(terpri)
  (princ source)
  (princ "result +++++++++++++++++++")(terpri)
  (run-commands nil (query-code source)))

(f "
common a b c;
var x a;
var y 'aaa';
collect x y c;
locked l s;
add a b c;
add 'a' 'b' 'c';
rm a b c;
collect l s;
symbol foo bar;
collect foo bar;
")

(f "
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
")

(f "
common root name has isa animal;
add name root 'root';
add name has 'has';
add name isa 'isa';
add name name 'name';
add name animal 'animal';
add has root animal;
symbol cat dog duck;
collect 'cat:' cat;
collect 'dog:' dog;
collect 'duck:' duck;
add name cat 'cat';
add name dog 'dog';
add name duck 'duck';
add isa cat animal;
add isa dog animal;
add isa duck animal;

findAll isa x animal;
findAll name x y;
collect 'animal name:' y;
")

(f "
findAll x y z;
collect x y z;
")

(f "
locked v s;
collect 'locked-sym:' v 'secret:' s;
unlock v s;
collect 'unlocked:)';
")

(finalize)
