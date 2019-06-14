(defpackage yami.parser
  (:use :cl
        :snaky)
  (:export :parse))
(in-package :yami.parser)

(defrule query
  (@ (many commands)))

(defrule commands
  (@ (or common
         var
         unlock
         symbol-command
         locked
         add
         rm
         find
         collect)))

(defrule common
  (and (cap "common") ws (many variable) ws ";"))

(defrule var
  (and (cap "var") ws variable ws (or symbol string) ws ";"))

(defrule unlock
  (and (cap "unlock") ws variable ws string ws ";"))

(defrule symbol-command
  (and (cap "symbol") ws (many variable) ws ";"))

(defrule locked
  (and (cap "locked") ws variable ws variable ws ";"))

(defrule add
  (and (cap "add") ws item ws item ws item ws ";"))

(defrule rm
  (and (cap "rm") ws item ws item ws item ws ";"))

(defrule find
  (and (cap (or "find" "find1" "findSome" "findAll")) ws item ws item ws item ws ";"))

(defrule collect
  (and (cap "collect") ws (many item) ws ";"))

(defrule item
  (or variable symbol string))

(defrule variable
  (@ (and (ret :variable) (cap (and (cc "a-zA-Z") (* (cc "a-zA-Z0-9_-")))))))

(defrule symbol
  (@ (and (ret :symbol) ":" (cap (+ (cc "a-zA-Z0-9_-"))))))

(defrule string
  (@ (or (and (ret :string)
              "\""
              (mod (@ (* (or (cap (cc "^\"\\"))
                             (and "\\n" (ret #.(format nil "~a" #\lf)))
                             (and "\\" (cap (any))))))
                   #'concat)
              "\"")
         (and (ret :string)
              "'"
              (mod (@ (* (or (cap (cc "^'\\"))
                             (and "\\n" (ret #.(format nil "~a" #\lf)))
                             (and "\\" (cap (any))))))
                   #'concat)
              "'"))))

(defun concat (list)
  (format nil "~{~a~}" list))

(defrule ws (grp "whitespace" (* (cc #.(format nil " ~a~a~a" #\cr #\lf #\tab)))))

(defrule (many x) (? (and x (* (and ws x)))))


(defparser parse (and ws query ws))

(print (parse "common x y; var z :a; add x y z; collect z;"))
