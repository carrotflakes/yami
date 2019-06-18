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
  :depends-on ("flexi-streams" "snaky")
  :components ((:module "src"
                :components
                ((:file "yami" :depends-on ("commands" "store" "query-log"))
                 (:file "commands" :depends-on ("parser" "string" "sym"))
                 (:file "parser")
                 (:file "store")
                 (:file "string")
                 (:file "sym")
                 (:file "query-log"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "yami-test"))))
