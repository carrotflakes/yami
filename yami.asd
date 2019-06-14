#|
  This file is a part of yami project.
  Copyright (c) 2019 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "yami"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("md5" "snaky")
  :components ((:module "src"
                :components
                ((:file "yami" :depends-on ("data" "find" "commands"))
                 (:file "find" :depends-on ("data"))
                 (:file "commands" :depends-on ("parser" "sym"))
                 (:file "parser")
                 (:file "data")
                 (:file "sym"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "yami-test"))))
