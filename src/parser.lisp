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
  (and (cap "common") _ (many variable) _ ";"))

(defrule var
  (and (cap "var") _ variable _ item _ ";"))

(defrule unlock
  (and (cap "unlock") _ variable _ (or string variable) _ ";"))

(defrule symbol-command
  (and (cap "symbol") _ (many variable) _ ";"))

(defrule locked
  (and (cap "locked") _ variable _ variable _ ";"))

(defrule add
  (and (cap "add") _ item _ item _ item _ ";"))

(defrule rm
  (and (ret "rm")
       (or (and "rm1" (ret 1)) (and "rmAll" (ret 10000000)))
       _ item _ item _ item _ ";"))

(defrule find
  (and (ret "find")
       (or (and "find1" (ret 1)) (and "findAll" (ret 10000000)))
       _ item _ item _ item _ ";"))

(defrule collect
  (and (cap "collect") _ (many item) _ ";"))

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

(defrule _
  (* (or ws comment)))

(defrule ws (grp "whitespace" (+ (cc #.(format nil " ~a~a~a" #\cr #\lf #\tab)))))

(defrule comment
  (grp "comment"
       (or (and "//" (* (cc #.(format nil "^~a~a" #\cr #\lf))))
           (and "/*" (* (and (! "*/") (any))) "*/"))))

(defrule (many x) (? (and x (* (and _ x)))))


(defparser parse (and _ query _))
